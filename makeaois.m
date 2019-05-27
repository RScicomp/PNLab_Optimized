FolderDomain = '/Users/Rgao/Desktop/McGill/McGillStuff/Pics/';
X1 = strcat(FolderDomain,'S_101_FF_66.jpg');
SetCalibParams; 

X = imread(X1);
X2 = imresize(X,[720,1280]);
imshow(X2);
axis equal;

axes('Visible', 'off', 'Units', 'normalized',...
    'Position', [0 0 1 1],...
    'DrawMode','fast',...
    'NextPlot','replacechildren');

xlim([1,Calib.mondims1.width]); ylim([1,Calib.mondims1.height]);axis ij;
hold on;
axis([0 1 0 1]); 
noaois = inputdlg({'No. AOIS:'});
noaois = str2num(noaois{1}) + 1;
a = 1;
aoinames = {};
while a < noaois
    axis equal;

    axes('Visible', 'off', 'Units', 'normalized',...
        'Position', [0 0 1 1],...
        'DrawMode','fast',...
        'NextPlot','replacechildren');

    xlim([1,Calib.mondims1.width]); ylim([1,Calib.mondims1.height]);axis ij;
    axis([0 1 0 1]); 
    hold on;
    x = getrect();
    %disp(x)
    aoinames(a) = inputdlg({'AOI name:'});
    if (a == 1)
        if (exist('aoi.csv', 'file')==2)
            delete('aoi.csv');
        end
        new = {x(1),x(1) + x(3), x(2),x(2)+x(4)};
        disp(new)
        dlmwrite('aoi.csv',{'X','X','Y','Y'},'delimiter',',','-append');
        dlmwrite('aoi.csv',new,'delimiter',',','-append');

    else 
        new = {x(1),x(1) + x(3), x(2),x(2)+x(4)};
        disp(new)
        dlmwrite('aoi.csv',new,'delimiter',',','-append');
        
    end
    a = a + 1;
    
end
cell2csv('aoinames.csv',aoinames,',');



clear;