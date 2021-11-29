function [allsubjsmat_thr, W_thr] = thresholdbasedonconsmat(allsubjsmat)
%Threshold all matrices in 3D connectivity matrix according to most
%consistent edges (across population)

%allsubjsmat = all unthresholded connectivity mats of population

%Load consistency scripts
addpath('/Users/alistairp/Documents/Toolbox/threshold-consist')

%Consistency thresholding
W_thr = threshold_consistency(allsubjsmat, 0.10);



%% Threshold each individual connectome
%According to consistency-matrix

nsubjs = size(allsubjsmat,3);

allsubjsmat_thr = zeros(512, 512, nsubjs);


for subj = 1:nsubjs
    
    tempsubjmat = allsubjsmat(:,:,subj);
    
    
    tempsubjmat(W_thr==0)=0;
    
    
    allsubjsmat_thr(:,:,subj) = tempsubjmat;
    
end


end