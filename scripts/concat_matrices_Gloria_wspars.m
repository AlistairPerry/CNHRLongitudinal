function [allsubjs] = concat_matrices_Gloria_wspars
%concat_matrices_Gloria Concatenates all subject matrices within a group
%folder
%NOTE - Call within the group folder

workingdirectory = pwd;
files = dir(workingdirectory);
dirFlags=[files.isdir];
subFolders=files(dirFlags);
subFolders(1:2)=[];

for s = 1:length(subFolders)
    currentSubj= subFolders(s,1).name;
    currentSubjDir = char([workingdirectory '/' currentSubj]);
    load([currentSubjDir '/' '10' '/' currentSubj '' 'metrics.mat']); %Matrix file containing information of each subject
    allsubjs(:,:,s) = SubjStruct.thr; %Matlab field representing connectivity matrix
end

end