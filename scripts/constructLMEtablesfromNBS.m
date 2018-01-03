function [LMEtable] = constructLMEtablesfromNBS(connectivityweights, grptimeinfo, ages, outputname)
%UNTITLED Summary of this function goes here
%   Detailed explanation goes here

nsubjs=(length(connectivityweights(:,1))./2);

fkIDS=1:nsubjs;
ID=cat(1,fkIDS',fkIDS');

LMEtable=table(ID,connectivityweights,grptimeinfo(:,1),grptimeinfo(:,2),ages);

LMEtable.Properties.VariableNames{'Var3'}='Group';
LMEtable.Properties.VariableNames{'Var4'}='Time';
LMEtable.Properties.VariableNames{'ages'}='Age';

writetable(LMEtable, [outputname '.txt']);

end

