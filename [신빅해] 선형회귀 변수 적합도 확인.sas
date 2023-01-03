PROC IMPORT datafile = "파일 위치.csv"
dbms = csv
out = dat;
getnames = Yes;
RUN;

/*컬럼 합치기 - schema_sub.xlsx 파일 참고*/
data dat2; set dat;
Bhouse = B1+B2+B3+B4+B5;
Btraffic = B7+B8+B9+B10+B11;
Bgoods = B13+B14+B15+B16+B17+B18+B19+B20+B21+B22+B23+B24+B25+B26+B27+B28+B29+B30+B31+B32+B57+B58+B59+B60+B78+B79;
Bonline = B33+B34;
Bfood = B35+B36+B37+B38+B39+B40+B41+B42;
Betc = B6+B43+B44+B45+B46+B47+B48+B49+B50+B51+B124+B125+B126+B127+B128+B136+B152+B153+B154;
Bfurniture = B52+B53+B134;
Bfee = B61+B62;
Bcloth = B63+B64+B65+B66+B67+B68+B69+B70+B71+B72+B73+B74+B75+B76;
Bcultural = B80+B81+B82+B83+B84+B85+B86+B87+B88+B89+B91+B92+B93+B95+B96+B121+B137+B138;
Bactivity = B90+B97+B98+B99+B100+B101+B102+B103+B104+B105+B120+B123;
Binsurance = B106+B107;
Bedu = B94+B108+B122+B155+B156+B160;
Bwedding = B109+B110+B111+B114+B115;
Bfuneral = B112+B116+B117;
Bcar = B118+B119+B133+B135+B157+B158+B159+B165+B166;
Bpro = B129+B130+B131+B132;
Bdoc = B139+B140+B141+B142+B143+B144+B145+B146+B147+B148+B149;
Bmanufactur = B150+B151;
Boil = B161+B162+B163+B164;
Btax = B12;
run;

/*단순선형회귀모델*/
proc reg data = dat2;
model B167 = Bhouse Btraffic Bgoods Bonline Bfood Betc Bfurniture Bfee Bcloth Bcultural Bactivity Binsurance Bedu Bwedding Bfuneral Bcar Bpro Bdoc Bmanufactur Boil Btax B54 B55 B56;
run;quit;

/*1차 자기회귀오차 적합*/
proc autoreg data = dat2;
model B167 = Bhouse Btraffic Bgoods Bonline Bfood Betc Bfurniture Bfee Bcloth Bcultural Bactivity Binsurance Bedu Bwedding Bfuneral Bcar Bpro Bdoc Bmanufactur Boil Btax B54 B55 B56 /nlag= 1 method = ml dwprob;
run;quit;
/*
ERROR: JVM(Java virtual machine) 예외입니다. java.lang.OutOfMemoryError: Java heap space.

WARNING: The marginal probabilities of Durbin-Watson cannot be obtained since you have too many observations or
         deficient data matrix.
*/
/*JAVA 경로 수정 권한 없어서 처리 불가*/

/*그룹별 변수 판단*/
/*1. Bhouse*/
proc reg data = dat2;
model B167 = B1 B2 B3 B4 B5;
run;quit;

/*1-2. Bhouse 1차 자기회귀오차적합*//*안됨!*/
proc autoreg data = dat2;
model B167 = B1 B2 B3 B4 B5 / method = ml dwprob;
run;quit;

/*2. Bgoods*/
proc reg data = dat2;
model B167 = B13 B14 B15 B16 B17 B18 B19 B20 B21 B22 B23 B24 B25 B26 B27 B28 B29 B30 B31 B32 B57 B58 B59 B60 B78 B79;
run;quit;

/*3. Betc*//**/
proc reg data = dat2;
model B167 = B6 B43 B44 B45 B46 B47 B48 B49 B50 B51 B124 B125 B126 B127 B128 B136 B152 B153 B154;
run;quit;

/*4. Bcloth*/
proc reg data = dat2;
model B167 = B63 B64 B65 B66 B67 B68 B69 B70 B71 B72 B73 B74 B75 B76;
run;quit;


/*5. Bcultural*/
proc reg data = dat2;
model B167 = B80 B81 B82 B83 B84 B85 B86 B87 B88 B89 B91 B92 B93 B95 B96 B121 B137 B138;
run;quit;


/*6. Bactivity*/
proc reg data = dat2;
model B167 = B90 B97 B98 B99 B100 B101 B102 B103 B104 B105 B120 B123;
run;quit;


/*7. Bcar*/
proc reg data = dat2;
model B167 = B118 B119 B133 B135 B157 B158 B159 B165 B166;
run;quit;


/*8. Bdoc*/
proc reg data = dat2;
model B167 = B139 B140 B141 B142 B143 B144 B145 B146 B147 B148 B149;
run;quit;
