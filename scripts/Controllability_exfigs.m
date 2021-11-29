%Create example brain visualisation of controllability calculations
%Forms Fig 3A in Roberts/Perry et al (in prep)

%Saves output in BrainNetViewer format


%% Panel A
%Derive consistency matrix to define representative (sparse) anatomical connectome

%Edges
W_thr = threshold_consistency(allsubjsnothr, 0.05);

Net_panelB = W_thr;
Net_panelB(Net_panelB~=0) = 0.5;

for i = 1:512
    if Net_panelB(136,i) ~= 0
        Net_panelB(136,i) = 3;
        Net_panelB(i,136) = 3;
    end
end


%Save connections of left hemisphere only

Net_panelB_LH = Net_panelB(1:256,1:256);

dlmwrite('LIFG_net5%cons_indentcons_LHonly.edge', Net_panelB_LH, 'delimiter', '\t', 'newline', 'pc');


%And left hemisphere nodes
%Point to coordinate file in github repos
COGfile = '/Users/alistairp/Documents/Brisbane/CNHRLongitudinal_dropbox/Data/COGnew.mat';

load(COGfile)

%Manually change LIFG/INS node to be more lateral
COG(136,1) = -60;

nodecol = ones(256,1);
nodesz = ones(256,1);

nodecol(136,1) = 2;
nodesz(136,1) = 3;

fid = fopen('all512_LH_panelA.node', 'wt');
for j = 1:256
    fprintf(fid, '%f\t%f\t%f\t%d\t%d\t%s\n', COG(j,1), COG(j,2), COG(j,3), nodecol(j,1), nodesz(j,1), '-');
end
fclose(fid);


%% Panel B
%No longer used

nodecol = ones(256,1);
nodesz = ones(256,1);

nodecol(136,1) = 2;
nodesz(136,1) = 3;

for i = 1:256
    
    if Net_panelB(136,i) ~= 0
        
        nodecol(i,1) = 3;
        nodesz(i,1) = 2;
        
    end
end

fid = fopen('all512_LH_panelB.node', 'wt');

for j = 1:256
    fprintf(fid, '%f\t%f\t%f\t%d\t%d\t%s\n', COG(j,1), COG(j,2), COG(j,3), nodecol(j,1), nodesz(j,1), '-');
end
fclose(fid);


%% Panel B
%Physical connections in NBS interaction network

Net_panelC = W_thr; 
Net_panelC(Net_panelC~=0) = 0.5;

Net_panelC_LHonly = Net_panelC(1:256,1:256);


%Output from NBS
nbsfile = '/Users/alistairp/Documents/Brisbane/CNHRLongitudinal_dropbox/NBS_interaction/NBS_output/Without_age_as_covariate/10%cons_5000p_InteractionNegativeAgeRemovedCovariateTh3.mat';
load(nbsfile);


%Derive nodes and connections in network

[xcol, ycol] = find(nbs.NBS.con_mat{1,1});

%remove RH nodes
xcol(8:end) = [];
ycol(8:end) = [];


nodes = cat(1,xcol,ycol);

nodes = unique(nodes);


parcnum = 512;

NBSnet = zeros(parcnum./2, parcnum./2);


for k = 1:length(xcol)
    NBSnet(xcol(k,1),ycol(k,1)) = 3;
    NBSnet(ycol(k,1),xcol(k,1)) = 3;
    
    %Remove pre-existing connections
    Net_panelC_LHonly(xcol(k,1),ycol(k,1)) = 0;
    Net_panelC_LHonly(ycol(k,1),xcol(k,1)) = 0;
    
end


Net_panelC_LHonly_plusNBSnet = Net_panelC_LHonly + NBSnet;

dlmwrite('LIFG_net5%cons_indentcons_LHonly_plusNBSnet.edge', Net_panelC_LHonly_plusNBSnet, 'delimiter', '\t', 'newline', 'pc');


%Write node attributes

nodecol = ones(256,1);

nodesz = ones(256,1);


nodecol(nodes(:,1)) = 2;

nodesz(nodes(:,1)) = 3;


%Load COG file again

load(COGfile);


fid = fopen('all512_LH_panelC.node', 'wt');

for j = 1:256
    fprintf(fid, '%f\t%f\t%f\t%d\t%d\t%s\n', COG(j,1), COG(j,2), COG(j,3), nodecol(j,1), nodesz(j,1), '-');
end
fclose(fid);


%Finished