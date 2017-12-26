function importconnectomes_basicanalysis(parcnum, parcname, thr, sparsity, varargin)
%Import connectome reps into one single structure for every subject
%Using new functions available within latest MRtrix package
%Alistair Perry, UNSW (2014)

workingdirectory = pwd;
files = dir(workingdirectory);
dirFlags=[files.isdir];
subFolders=files(dirFlags);
subFolders(1:2)=[];

for s = 1:length(subFolders)
    currentSubj= subFolders(s,1).name;
    currentSubjDir = char([workingdirectory '/' currentSubj]);
    
    %setup output directory
    
    outdirname=int2str(sparsity);
    mkdir([currentSubjDir '/' outdirname]);
    
    if thr==1
    
    countfile = dir(fullfile(currentSubjDir, '*count*'));
    countmtx = dlmread([currentSubjDir '/' countfile.name]);
    
    invcountfile = dir(fullfile(currentSubjDir, '*invlengths*'));
    invcountmtx = dlmread([currentSubjDir '/' invcountfile.name]);
    
    invnodelengthfile = dir(fullfile(currentSubjDir, '*invnodeandlengths*'));
    invnodelengthmtx = dlmread([currentSubjDir '/' invnodelengthfile.name]);
    
    lengthfile = dir(fullfile(currentSubjDir, '*tracklengths*'));
    lengthmtx = dlmread([currentSubjDir '/' lengthfile.name]);
    
    %parcload = load_untouch_nii([commondir '/' currentSubj '/' parcname '.nii']); 
    
    for i = 1:parcnum
        for j = i+1:parcnum
            if i == parcnum break; end
        countmtx(j,i) = countmtx(i,j);
        lengthmtx(j,i) = lengthmtx(i,j);
        invcountmtx(j,i)=invcountmtx(i,j);
        invnodelengthmtx(j,i)=invnodelengthmtx(i,j);
        end
    end
    
    SubjStruct=struct;
    
    SubjStruct.ORG=countmtx;
    SubjStruct.ORGinv=invcountmtx;
    SubjStruct.ORGinvnodelength=invnodelengthmtx;
    SubjStruct.tckdistmat=lengthmtx;
    
    %BrainMask = load_untouch_nii([currentSubjDir '/' 'biasb0brain_mask.nii']);
    %SubjStruct.BrainSize = numel(find(BrainMask.img~=0));
    
    [rois, ~] = extract_roi([currentSubjDir '/' parcname '' '.nii']);
    SubjStruct.distmat = zeros(parcnum, parcnum);
        
        for i = 1:parcnum
            for j = 1:parcnum
                SubjStruct.distmat(j, i) = sqrt(abs([(rois(i,1)-rois(j,1))*(rois(i,1)-rois(j,1))]+[(rois(i,2)-rois(j,2))*(rois(i,2)-rois(j,2))]+[(rois(i,3)-rois(j,3))*(rois(i,3)-rois(j,3))]));
            end
        end
        
    if ~isempty(varargin)

    normtype=varargin{1};   
    
    SubjStruct.thr = threshold_proportional(['SubjStruct.ORG' normtype], (sparsity./100));
        
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
       
    %Basic nodal connectivity info
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
    
    %Distance Calcs
    numbercon = nnz(SubjStruct.CIJ);
    SubjStruct.totalDists = sum(sum(SubjStruct.thrdistmat));
    SubjStruct.MAD = SubjStruct.totalDists./numbercon;
    
    fprintf('\n %s completed \n' , currentSubj);
    
    %Save output
    save([currentSubjDir '/' outdirname '/' currentSubj '' 'metrics.mat'], 'SubjStruct');
end
end


