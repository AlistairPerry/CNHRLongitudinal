function basicanalysis_consistencythr(consmatrix, origsparsity, outdirname)
%Import connectome reps into one single structure for every subject
%Using new functions available within latest MRtrix package
%Alistair Perry, UNSW (2014)

workingdirectory = pwd;
files = dir(workingdirectory);
dirFlags=[files.isdir];
subFolders=files(dirFlags);
subFolders(1:2)=[];

%read in sparsity
origsparsity=int2str(origsparsity);

for s = 1:length(subFolders)
    currentSubj= subFolders(s,1).name;
    currentSubjDir = char([workingdirectory '/' currentSubj]);
    
    %setup output directory
    
    mkdir([currentSubjDir '/' outdirname]);
    
    %load original matrix and decompose into consistent edges
    
    load([currentSubjDir '/' origsparsity '/' currentSubj  '' 'metrics.mat']);
    
    subjconsmatrix=SubjStruct.ORGinv;
    subjconsmatrix(consmatrix==0)=0;
    
    subjconsdistmatrix=SubjStruct.tckdistmat;
    subjconsdistmatrix(consmatrix==0)=0;
    
    %convert back into new SubjStruct
    
    SubjStruct=struct;
    SubjStruct.thr = subjconsmatrix;
    SubjStruct.thrdistmat = subjconsdistmatrix;
    
    SubjStruct.CIJ = weight_conversion(SubjStruct.thr, 'binarize');    
    
    %Basic nodal connectivity info
    %SubjStruct.DEG = degrees_und(SubjStruct.CIJ);
    SubjStruct.STR = strengths_und(SubjStruct.thr);
    %SubjStruct.BETC = betweenness_bin(SubjStruct.thr);
    
    %Eff
    %CIJd = distance_bin(SubjStruct.CIJ);
    %SubjStruct.CPL = charpath(CIJd);
    %SubjStruct.EFF = efficiency_bin(SubjStruct.CIJ);
    
    %Local efficiency
    %SubjStruct.NodalEff=efficiency_bin(SubjStruct.CIJ,1);
    %SubjStruct.NodalEff(isinf(SubjStruct.NodalEff))=0;
    
    %Clustering
    %SubjStruct.CCOEFF = clustering_coef_bu(SubjStruct.CIJ);
    %SubjStruct.avgCCOEFF = mean(SubjStruct.CCOEFF);
    
    %Distance Calcs
    numbercon = nnz(SubjStruct.CIJ);
    SubjStruct.totalDists = sum(sum(SubjStruct.thrdistmat));
    SubjStruct.MAD = SubjStruct.totalDists./numbercon;
    
    fprintf('\n %s completed \n' , currentSubj);
    
    %Save output
    save([currentSubjDir '/' outdirname '/' currentSubj '' 'metrics.mat'], 'SubjStruct');
end
end


