function [allsubjsmat_thr, W_thr] = thresholdbasedonconsmat(allsubjsmat)
%Threshold all matrices in 3D connectivity matrix according to most
%consistent edges (across population)

addpath('/Users/alistairp/Documents/Toolbox/threshold-consist')

W_thr = threshold_consistency(allsubjsmat, 0.10);


nsubjs = size(allsubjsmat,3);

allsubjsmat_thr = zeros(512, 512, nsubjs);


for subj = 1:nsubjs
    
    tempsubjmat = allsubjsmat(:,:,subj);
    
    
    tempsubjmat(W_thr==0)=0;
    
    
    allsubjsmat_thr(:,:,subj) = tempsubjmat;
    
end


end