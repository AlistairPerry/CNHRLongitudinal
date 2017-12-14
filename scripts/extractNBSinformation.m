function [NBSweightssum]=extractNBSinformation(nbsfile, connectivitymatrices, parcstrings, COG, basefilename, varargin)

load(nbsfile);
nbstab = [];
nbscell = cell(1,2);

parc=length(nbs.NBS.test_stat);
numsubjs=size(connectivitymatrices,3);

%extract subject connectivity weights

for i = 1:length(nbs.NBS.con_mat(1,:))
    NBSweights=[];
    [xcol, ycol] = find(nbs.NBS.con_mat{1,i});
    for k = 1:length(xcol)
        NBSweights(:,k)=squeeze(connectivitymatrices(xcol(k,1),ycol(k,1),:));
        NBSweightssum(:,1)=sum(NBSweights,2);
    end
end

dlmwrite([basefilename '' 'subjweights.txt'], NBSweightssum, 'delimiter', '\t');

%now extract output tables

for i = 1:length(nbs.NBS.con_mat(1,:))
    [xcol, ycol] = find(nbs.NBS.con_mat{1,i});
    for k = 1:length(xcol(:,1))
        if isempty(nbstab)
            nbstab(1,1) = i;
            nbstab(1,2) = xcol(k,1);
            nbstab(1,3) = ycol(k,1);
            nbscell(1,1) = parcstrings(xcol(k,1), 1);
            nbscell(1,2) = parcstrings(ycol(k,1), 1);
        else
            nbstab(numel(nbstab(:,1))+1, 1) = i;
            nbstab(numel(nbstab(:,2)), 2) = xcol(k,1);
            nbstab(numel(nbstab(:,3)), 3) = ycol(k,1);
            nbscell(numel(nbscell(:,1))+1,1) = {parcstrings{xcol(k,1), 1}};
            nbscell(numel(nbscell(:,2)), 2) = {parcstrings{ycol(k,1), 1}};
        end
    end
end

fid = fopen([basefilename '' 'results.txt'], 'wt');
for i = 1:length(nbstab(:,1))
    fprintf(fid, '%d\t%d\t%d\t%s\t%s\n', nbstab(i,1), nbstab(i,2), nbstab(i,3), nbscell{i,1}, nbscell{i,2});
end
fclose(fid)

%option to load in apriori information of network nodes
if ~isempty(varargin)

        nodeindex=varargin{1};
        netlabels=varargin{2};
    
    for i = 1:length(nbs.NBS.con_mat(1,:))
        
        xindex=nodeindex(nbstab(:,2),1);
        yindex=nodeindex(nbstab(:,3),1);
        
        netNBSmat=zeros(max(nodeindex),max(nodeindex));
        
        for x = 1:length(xindex)
            netNBSmat(xindex(x),yindex(x))=netNBSmat(xindex(x),yindex(x))+1;
            netNBSmat(yindex(x),xindex(x))=netNBSmat(yindex(x),xindex(x))+1;
        end
        
        netNBSmatl=triu(logical(ones(size(netNBSmat))));
        netNBSmat(~netNBSmatl)=nan;
        heatmap(netNBSmat,netlabels,netlabels,'','GridLines','none', 'NanColor',[1 1 1]);
       
        c=colorbar;
        ylabel(c,'Number of edges');
        
        savefig([basefilename '' int2str(i) 'Net.fig']);
    
    end
end

%Create output for BNV

%subnetworks
%nodes
for i = 1:length(nbs.NBS.con_mat(1,:))
    [xcol, ycol] = find(nbs.NBS.con_mat{1,i});
    nodes = cat(1,xcol,ycol);
    nodes = unique(nodes);
    
    fid = fopen([basefilename '' int2str(i) '' '.node'], 'wt');
    for j = 1:length(nodes)
        fprintf(fid, '%f\t%f\t%f\t%d\t%d\t%s\n', COG(nodes(j,1),1), COG(nodes(j,1),2), COG(nodes(j,1),3), 1, 1, '-');
    end
end
fclose(fid)

%edges
fkmtx = zeros(length(xcol),length(xcol));
for k = 1:length(xcol)
    [iloc,~] = find(xcol(k,1)==nodes);
    [jloc,~] = find(ycol(k,1)==nodes);
    fkmtx(iloc,jloc) = 1;
    fkmtx(jloc,iloc) = 1;
end
dlmwrite([basefilename '' int2str(i) '' '.edge'], fkmtx, 'delimiter', '\t', 'newline', 'pc');

end