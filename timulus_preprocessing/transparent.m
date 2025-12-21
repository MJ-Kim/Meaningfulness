% Silhouette transparent, no resize
% 디렉토리 설정
infile = 'diffeomorphic30';
outfile = 'diffeomorphic30_trans';

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

    gray = im2gray(img);

    [~, imgName, ~] = fileparts(imgList(i).name);
    % outName = fullfile(outfile, [imgName '.png']);
    outName = fullfile(outfile, [imgName '_scram.png']);
    imwrite(gray, outName, Alpha=gray);
end
   
