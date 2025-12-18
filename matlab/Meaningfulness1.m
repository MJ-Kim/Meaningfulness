% Meaningfulness1.m
% 

cd('~/Dropbox/2017Experiment/Meaningfulness');
work_dir = pwd;

ClockRandSeed;


% 설계 =====================================================================
TRG = 4; % 물체 4개 중 검사할 물체 위치 
KEY = 2; % 정답 위치: 좌, 우
REP = 4; % 연습 제외 32시행
nTRIAL = TRG * KEY * REP;

xIndex = 0:(nTRIAL-1);

cTrg = xIndex'; % 각 사분면 = 샘플로 제시되는 네 자극의 위치
cTrg(mod(xIndex, TRG)==0) = 1;
cTrg(mod(xIndex, TRG)==1) = 2;
cTrg(mod(xIndex, TRG)==2) = 3;
cTrg(mod(xIndex, TRG)==3) = 4;

cKey = xIndex'; % 기억 검사에서 샘플과 일치하는 옵션의 위치
cKey(mod(fix(xIndex/(TRG)), KEY)==0) = 1;	% left
cKey(mod(fix(xIndex/(TRG)), KEY)==1) = 2;	% right
% [xIndex', cTrg, cKey]


% 색깔 인덱스 ================================================================
idxCol = cell(1,2); % 역균형 할 위치세트
idxCol{1} = zeros(36, 5); % 연습 4 + 본시행 32; 각 사분면에 나올 색깔 + foil 색깔
idxCol{2} = zeros(36, 5);

idxTrg = cell(1,2); % 사분면 중 표적의 위치
idxTrg{1} = mod((1:36)-1,4)' + 1;
idxTrg{2} = mod((1:36)-1,4)' + 1;

for mm=1:2
	for nn=1:36

		go = 1;
		while go
			A = randperm(360,4); % 360가지 RGB 중 4개를 뽑아서
			sortA = sort(A); % 크기 순서로 나열한다.
			
			diffA = mod(sortA([2:4,1]) - sortA, 360); % 증가량을 계산하고
		
			if sum(diffA > 30)==4 % 모든 증가량이 30도보다 크면
				go = 0; % 랜덤추출을 멈춘다.

				A(end+1) = mod(A(idxTrg{mm}(nn))+180-1, 360)+1; % foil 계산
				idxCol{mm}(nn,:) = A; % 저장한다.
			end
		end
	end
end
% [idxCol{1}, idxTrg{1}]
% [idxCol{2}, idxTrg{2}]


cc = load("colorwheel360"); % Memtoolbox에서 제공하는 360가지 RGB 값.
cc = cc.fullcolormatrix; % 행렬만 남긴다.


% Spreadsheet =============================================================
% 총 8가지 spreadsheets가 필요하다. 아래 참조.
for orderGrp = 1:2
for stmGrp = 1:2
for colGrp = 1:2

% orderGrp = 1;
orderList = {'CON', 'DIS'}; % conjunction 먼저, disjunction 나중에.
if orderGrp == 2
	orderList = {'DIS', 'CON'}; % disjunction 먼저, conjunction 나중에.
end

% stmGrp = 1;
stmType = 'Intact'; % intact 집단
stmSuffix = ""; % png 파일명 추가 없음
if stmGrp == 2
	stmType = 'Diffeo'; % diffeomorphic scrambled 집단
	stmSuffix = "_scram";  % png 파일명 뒤에 붙는다.
end

% colGrp = 1;
orderCol = [1, 2]; % 색깔 인덱스. 예; orderGrp==1일 때, CON은 idxCol{1}, DIS는 idxCol{2}
if colGrp == 2
	orderCol = [2, 1]; % 예; orderGrp==1일 때, CON은 idxCol{2}, DIS는 idxCol{1}
end


% 물체, 반응 ================================================================
fPNG = dir('intact/*.png');
fPNG = sort({fPNG.name}); % abc 순서로 배열한다.
idxPNG = reshape(1:288,72,4); % 동일한 하위범주 물체들이 한 시행에서 동시에 등장하지 않도록, 고릴라 무선화 기능을 고려하여 인덱싱.

rspTxt = {'left', 'right'};

% 파일 =====================================================================

% SpreadSheet version 1~8.
SSver = sprintf("v%02d%s%sc%d", (stmGrp-1)*4 + (orderGrp-1)*2 + colGrp, stmType, orderList{1}, orderCol(1));
dataFile = fopen(fullfile(work_dir, sprintf("%s.csv", SSver)), 'w');

% Write header
fprintf(dataFile,'SSver,display,case,maxn,trial,object1,object2,object3,object4,color1,color2,color3,color4,');
fprintf(dataFile,'trgLoc,target,key,left,right,procedure\n');


% Intro
fprintf(dataFile, sprintf('%s,Introduction\n', SSver));



for cntBlk = 1:2 % CON->DIS or DIS->CON

cntCase = 0; % 1~36.

% PracIntro1 ==============================================================
fprintf(dataFile, sprintf('%s,%spracIntro,,,,,,,,,,,,,,,,,proc%s%s.png\n', ...
	SSver, orderList{cntBlk}, stmType, orderList{cntBlk}));


% PracTrial1 ==============================================================
xKey = Shuffle([1,1,2,2]); % 연습 4시행에서 sample 위치와 probe 위치 관계를 무선화(역균형 불가).

for it = 1:4
	cntCase = cntCase + 1;

	% SSver,display,block,case,maxn,trial
	fprintf(dataFile, sprintf('%s,%sprac,%d,4,%d,', SSver, orderList{cntBlk}, cntCase, it));

	% obj1,obj2,obj3,obj4
	for mm=1:4
		tmp = fPNG{idxPNG(cntCase + (cntBlk-1)*36, mm)};
		fprintf(dataFile, sprintf('%s%s.png,', tmp(1:end-4), stmSuffix));
	end

	% col1,col2,col3,col4
	for mm=1:4
		xcol = cc(idxCol{orderCol(cntBlk)}(cntCase, mm),:);
		fprintf(dataFile, sprintf('rgb(%3.1f %3.1f %3.1f),', xcol(1), xcol(2), xcol(3)));
	end

	% trgLoc, target
	% fprintf(dataFile, sprintf('%d,%s,', idxTrg{orderCol(cntBlk)}(cntCase), fPNG{idxPNG(cntCase, idxTrg{orderCol(cntBlk)}(cntCase))}));
	fprintf(dataFile, sprintf('%d,object%d,', idxTrg{orderCol(cntBlk)}(cntCase), idxTrg{orderCol(cntBlk)}(cntCase)));

	% pCol1,pCol2
	if xKey(it)==1
		xopt = [idxCol{orderCol(cntBlk)}(cntCase, idxTrg{orderCol(cntBlk)}(cntCase)), idxCol{orderCol(cntBlk)}(cntCase, 5)];
	else
		xopt = [idxCol{orderCol(cntBlk)}(cntCase, 5), idxCol{orderCol(cntBlk)}(cntCase, idxTrg{orderCol(cntBlk)}(cntCase))];
	end
	fprintf(dataFile, sprintf('%s,', rspTxt{xKey(it)}));
	for mm=1:2
		xcol = cc(xopt(mm),:);
		fprintf(dataFile, sprintf('rgb(%3.1f %3.1f %3.1f),', xcol(1), xcol(2), xcol(3)));
	end

	fprintf(dataFile, '\n'); 
end

% MainIntro1 ==============================================================
fprintf(dataFile, sprintf('%s,%smainIntro\n', SSver, orderList{cntBlk}));


% MainTrial1 ==============================================================
for it = 1:nTRIAL
	xTrg = cTrg(it);
	xKey = cKey(it);

	cntCase = cntCase + 1;

	% SSver,display,block,case,maxn,trial
	fprintf(dataFile, sprintf('%s,%smain,%d,%d,%d,', SSver, orderList{cntBlk}, cntCase, nTRIAL, it));

	% obj1,obj2,obj3,obj4
	for mm=1:4
		tmp = fPNG{idxPNG(cntCase + (cntBlk-1)*36, mm)};
		fprintf(dataFile, sprintf('%s%s.png,', tmp(1:end-4), stmSuffix));
	end

	% col1,col2,col3,col4
	for mm=1:4
		xcol = cc(idxCol{orderCol(cntBlk)}(cntCase, mm),:);
		fprintf(dataFile, sprintf('rgb(%3.1f %3.1f %3.1f),', xcol(1), xcol(2), xcol(3)));
	end

	% trgLoc, target
	% fprintf(dataFile, sprintf('%d,%s,', idxTrg{orderCol(cntBlk)}(cntCase), fPNG{idxPNG(cntCase, idxTrg{orderCol(cntBlk)}(cntCase))}));
	fprintf(dataFile, sprintf('%d,object%d,', idxTrg{orderCol(cntBlk)}(cntCase), idxTrg{orderCol(cntBlk)}(cntCase)));

	if xTrg ~= idxTrg{orderCol(cntBlk)}(cntCase)
		error("Target positions do not match.")
	end

	% pCol1,pCol2
	if xKey==1
		xopt = [idxCol{orderCol(cntBlk)}(cntCase, idxTrg{orderCol(cntBlk)}(cntCase)), idxCol{orderCol(cntBlk)}(cntCase, 5)];
	else
		xopt = [idxCol{orderCol(cntBlk)}(cntCase, 5), idxCol{orderCol(cntBlk)}(cntCase, idxTrg{orderCol(cntBlk)}(cntCase))];
	end
	fprintf(dataFile, sprintf('%s,', rspTxt{xKey}));
	for mm=1:2
		xcol = cc(xopt(mm),:);
		fprintf(dataFile, sprintf('rgb(%3.1f %3.1f %3.1f),', xcol(1), xcol(2), xcol(3)));
	end

	fprintf(dataFile, '\n'); 
end

end

fprintf(dataFile,sprintf('%s,Bye',SSver)); 

fclose('all');


end
end
end
	

