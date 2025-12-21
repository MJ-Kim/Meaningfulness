% Silhouette transparent, resize
% 디렉토리 설정
infile = 'Silhouette_objects';
outfile = 'resize_trans';

%create output directory
if ~exist(outfile, 'dir')
    mkdir(outfile);
end

% 모든 이미지 불러오기
imgList = dir(fullfile(infile,'*.png'));
% imgList = dir(fullfile(infile,'gas-station.png'));

% 이미지 처리
for i = 1:length(imgList)
    img = imread(fullfile(infile, imgList(i).name));

    % 이미지 비율 구하기
    [height, width, ~] = size(img);
    ratio = width / height;
    
    % 이미지 크기 및 위치 조정
    if width >= height
        new_width = 400;
        new_height = round(new_width / ratio);
        % 이미지 위치 조정
        x = 50;
        y = (500 - new_height)/2;
    else
        new_height = 400;
        new_width = round(new_height * ratio);
        x = (500 - new_width)/2;
        y = 50;
    end

    % Resize
    resized_img = imresize(img, [new_height, new_width]);
    resized_img = imtranslate(resized_img, [x, y], OutputView = 'full', FillValues=255);
    resized_img = im2gray(resized_img);
    
    % 흰색 배경 생성
    bkg = uint8(ones(500, 500) * 255); % 255=WHITE.
    
    % 배경과 이미지 합치기
    result_img = imfuse(bkg, resized_img, 'blend');
    
    map = make_map(result_img);
        
    [~, imgName, ~] = fileparts(imgList(i).name);
    outName = fullfile(outfile, [imgName '.png']);
    % outName = fullfile(outfile, [imgName '_resize.png']);
    imwrite(result_img, outName, Alpha=map);
end

function m = make_map(img)
    [rows, cols, ~] = size(img);
    m = img;
    for row = 1:rows
        for col = 1:cols
            if m(row, col) < 200
                m(row, col) = m(row, col)/40;
            end
        end
    end
end
    
