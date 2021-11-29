function importconnectomes_basicanalysis(parcnum, parcname, thr, sparsity, varargin)
%Import raw connectomes and perform basic network analyses
%Based upon data files and forms extracted from MRtrix tck2connectome
%Loads in raw track counts, inverted counts (for distance), and track
%lengths
%Using new functions available within latest MRtrix package
%Alistair Perry, University of Cambridge (2021)


%Note, this is used for generating over connectome structure but not in any
%subsequent analysis


%Run function from directory containing all scans to be analysed

workingdirectory = pwd;
files = dir(workingdirectory);
dirFlags=[files.isdir];
subFolders=files(dirFlags);
subFolders(1:2)=[];


%Loop through all subjects
for s = 1:length(subFolders)
    currentSubj= subFolders(s,1).name;
    currentSubjDir = char([workingdirectory '/' currentSubj]);
    
    %setup output directory
    
    outdirname=int2str(sparsity);
    mkdir([currentSubjDir '/' outdirname]);
    
    if thr==1
        
        %Raw connectome weights
        countfile = dir(fullfile(currentSubjDir, '*count*'));
        countmtx = dlmread([currentSubjDir '/' countfile.name], '%f', 0, 0, [0 0 (parcnum-1) (parcnum-1)]);
        
        %Inverted weights
        invcountfile = dir(fullfile(currentSubjDir, '*invlengths*'));
        invcountmtx = dlmread([currentSubjDir '/' invcountfile.name], '%f', 0, 0, [0 0 (parcnum-1) (parcnum-1)]);
        
        %Physical fiber lengths
        lengthfile = dir(fullfile(currentSubjDir, '*tracklengths*'));
        lengthmtx = dlmread([currentSubjDir '/' lengthfile.name], '%f', 0, 0, [0 0 (parcnum-1) (parcnum-1)]);
                
        for i = 1:parcnum
            for j = i+1:parcnum
                if i == parcnum break; end
                countmtx(j,i) = countmtx(i,j);
                lengthmtx(j,i) = lengthmtx(i,j);
                invcountmtx(j,i)=invcountmtx(i,j);
            end
        end
        
        
        %Start to build connectome structure output
        SubjStruct=struct;
        
        SubjStruct.ORG=countmtx;
        SubjStruct.ORGinv=invcountmtx;
        SubjStruct.tckdistmat=lengthmtx;
        
        
        [rois, ~] = extract_roi([currentSubjDir '/' parcname '' '.nii']);
        SubjStruct.distmat = zeros(parcnum, parcnum);
        
        for i = 1:parcnum
            for j = 1:parcnum
                SubjStruct.distmat(j, i) = sqrt(abs([(rois(i,1)-rois(j,1))*(rois(i,1)-rois(j,1))]+[(rois(i,2)-rois(j,2))*(rois(i,2)-rois(j,2))]+[(rois(i,3)-rois(j,3))*(rois(i,3)-rois(j,3))]));
            end
        end
       
        
        %IF thresholding connectome to traditional proportional approach
        if ~isempty(varargin)
            
            normtype=varargin{1};
            
            SubjStruct.thr = threshold_proportional(SubjStruct.(normtype), (sparsity./100));
            
        else
            
            SubjStruct.thr = threshold_proportional(SubjStruct.ORGinv, (sparsity./100));
            
        end
        
        SubjStruct.CIJ = weight_conversion(SubjStruct.thr, 'binarize');
        
        save([currentSubjDir '/' outdirname '/' currentSubj '' 'metrics.mat'], 'SubjStruct');
        
    end
    
    load([currentSubjDir '/' outdirname '/' currentSubj  '' 'metrics.mat']);
    
    %Distance info
    SubjStruct.thrdistmat = zeros(parcnum,parcnum);
    for i = 1:parcnum
        for j = 1:parcnum
            if SubjStruct.CIJ(i,j) == 1
                SubjStruct.thrdistmat(i,j) = SubjStruct.tckdistmat(i,j);
                SubjStruct.thrdistmat(j,i) = SubjStruct.tckdistmat(j,i);
            end
        end
    end
    
    SubjStruct.numfibers = sum(sum(SubjStruct.ORGinv));
    
    %% Basic nodal connectivity info
    SubjStruct.DEG = degrees_und(SubjStruct.CIJ);
    SubjStruct.STR = strengths_und(SubjStruct.thr);
    SubjStruct.BETC = betweenness_bin(SubjStruct.thr);
    
    %Eff
    CIJd = distance_bin(SubjStruct.CIJ);
    SubjStruct.CPL = charpath(CIJd);
    SubjStruct.EFF = efficiency_bin(SubjStruct.CIJ);
    
    %Local efficiency
    SubjStruct.NodalEff=efficiency_bin(SubjStruct.CIJ,1);
    SubjStruct.NodalEff(isinf(SubjStruct.NodalEff))=0;
    
    %Clustering
    SubjStruct.CCOEFF = clustering_coef_bu(SubjStruct.CIJ);
    SubjStruct.avgCCOEFF = mean(SubjStruct.CCOEFF);
    
    %Communicability
    
    COMM = communicability_bu(SubjStruct.CIJ);
    SubjStruct.TCOMM = sum(sum(triu(COMM)));
    
    %Distance Calcs
    numbercon = nnz(SubjStruct.CIJ);
    SubjStruct.totalDists = sum(sum(SubjStruct.thrdistmat));
    SubjStruct.MAD = SubjStruct.totalDists./numbercon;
    
    fprintf('\n %s completed \n' , currentSubj);
    
    %Save output
    save([currentSubjDir '/' outdirname '/' currentSubj '' 'metrics.mat'], 'SubjStruct');
    
end
end