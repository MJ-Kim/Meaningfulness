% Silhouette resize
% 디렉토리 설정
infile = 'Silhouette_objects';
outfile = '400size';

%create output directory
if ~exist(outfile, 'dir')
    mkdir(outfile);
end

% 모든 이미지 불러오기
imgList = dir(fullfile(infile,'*.png'));
% imgList = dir(fullfile(infile,'gas-station.png'));

% 이미지 처리
for i =1:length(imgList)
    img = imread(fullfile(infile, imgList(i).name));

    % 이미지 비율 구하기
    [height, width, ~] = size(img);
    ratio = width / height;
    
    % 이미지 크기 및 위치 조정
    if width >= height
        new_width = 400;
        new_height = round(new_width / ratio);
        
    else
        new_height = 400;
        new_width = round(new_height * ratio);
   
    end

    % Resize
    resized_img = imresize(img, [new_height, new_width]);
        
    [~, imgName, ~] = fileparts(imgList(i).name);
    outName = fullfile(outfile, [imgName '.png']);
    % outName = fullfile(outfile, [imgName '_resize.png']);
    imwrite(resized_img, outName);
end
