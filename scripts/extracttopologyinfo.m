function [ConTable] = extracttopologyinfo(textfilename, sparsity, outputtablename)
%UNTITLED Summary of this function goes here
%   Detailed explanation goes here

workingdirectory = pwd;
files = dir(workingdirectory);
dirFlags=[files.isdir];
subFolders=files(dirFlags);
subFolders(1:2)=[];
subjs=textread([textfilename],'%s');

%create ID for mixed effects purposes
ntrusubjs=(length(subjs(:,1))./2);
fkIDS=1:ntrusubjs;
ID=cat(1,fkIDS',fkIDS');

for s = 1:length(subjs)
currentSubj = subjs{s,1};
currentSubjDir = char([workingdirectory '/' currentSubj]);
load([currentSubjDir '/' int2str(sparsity) '/' currentSubj '' 'metrics.mat']); %Matrix file containing information of each subject

%load whole brain topology metrices
CPL(s,1) = SubjStruct.CPL; 
EFF(s,1) = SubjStruct.EFF;
CC(s,1) = SubjStruct.avgCCOEFF;

%inter-hemispheric connectivity
parcnum=numelements(SubjStruct.thr);

%assume that parc integers are asymmetric
InterHemC(s,1)=sum(sum(SubjStruct.thr(1:(parcnum./2),(parcnum./2)+1:parcnum)));

end

ConTable=table(subjs, ID, CPL, EFF, CC, InterHemC);

writetable(ConTable, [outputtablename '.txt']);

end

