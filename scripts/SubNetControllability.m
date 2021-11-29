function [subnetconvals_all] = SubNetControllability(connectivitymats, nbsfile)
%Calculate subnetwork controllability for NBS network 
%By calculating controllabilities for all regions in NBS network

%Inputs:
%Connectivitymats - 3D matrix of all subjects and imaging timepoints
%nbsfile: NBS-derived output file

%% Setup
%Load paths

%Controllability code
%https://complexsystemsupenn.com/s/controllability_code-smb8.zip

addpath('/Users/alistairp/Documents/Toolbox/controllability_code');


%Output DIR

OUTDIR = '/Users/alistairp/Documents/Brisbane/CNHRLongitudinal_dropbox/NBS_interaction/Output';


%Setup subnetwork control nodes

load(nbsfile);

[xcol, ycol] = find(nbs.NBS.con_mat{1,1});
nodes = cat(1,xcol,ycol);
nodes = unique(nodes);


%% Loop through subjects

nscans = size(connectivitymats,3);

for i = 1:nscans
    
    
    %read in connectivity mats
    
    subjmat = connectivitymats(:,:,i);
    
    
    %Normalization
    
    [~, D, ~] = eigs(subjmat, 1);
    
    Dall(i,1) = D;
    
end


%Used all subjects eigs to determine largest eigenvalue and scaling number

maxD=max(Dall);

tenpercD=0.10*maxD;
ScalDNum = maxD+tenpercD;


%Now scaling of all connectivity weights
%And calc of all controllability values

for i = 1:nscans
    
    subjmat = connectivitymats(:,:,i);
    
    subjmatscal = subjmat./ScalDNum;
    
    convals = ave_control(subjmatscal);
    
    
    subnetconvals = sum(convals(nodes));
    
    subnetconvals_all(i,1) = subnetconvals;
    
    
end


%Save output

dlmwrite([OUTDIR '/' 'subnetcon_nbsint_t3negcon_ageremov.txt'], subnetconvals_all);


end