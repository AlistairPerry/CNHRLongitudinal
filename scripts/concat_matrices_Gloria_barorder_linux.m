function [output,subjs] = concat_matrices_Gloria_barorder_linux(textfilename, sparsity)
%UNTITLED Summary of this function goes here
%   Detailed explanation goes here

workingdirectory = pwd;
files = dir(workingdirectory);
dirFlags=[files.isdir];
subFolders=files(dirFlags);
subFolders(1:2)=[];
subjs=textread([textfilename],'%s');

for s = 1:length(subjs)
currentSubj = subjs{s,1};
currentSubjDir = char([workingdirectory '/' currentSubj]);
load([currentSubjDir '/' sparsity '/' currentSubj '' 'metrics.mat']); %Matrix file containing information of each subject
output(:,:,s) = SubjStruct.thr; %Matlab field representing connectivity matrix
end

end

