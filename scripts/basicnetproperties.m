function basicnetproperties(connectivitymats)
%Calculate basic network properties
%Calculate total strength and network density from raw connectomes


nscans = size(connectivitymats,3);

nparcs = 512;


OUTDIR = '/Users/alistairp/Documents/Brisbane/CNHRLongitudinal_dropbox/Misc';


for i = 1:nscans
    
    
    %read in connectivity mats
    
    subjmat = connectivitymats(:,:,i);
    
    
    %Calculate basic properties
    
    totalstr(i,1) = sum(sum(subjmat));
    
    totaldens(i,1) = [nnz(subjmat)./(nparcs*(nparcs-1))]*100;
    
    
end


%Save output

dlmwrite([OUTDIR '/' 'allsubjs_netstr.txt'], totalstr);

dlmwrite([OUTDIR '/' 'allsubjs_netdens.txt'], totaldens);


end