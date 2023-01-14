import pandas as pd
import numpy as np

#파일 불러오기
csv = pd.read_csv(r"C:\Users\강동주\Desktop\파이썬\leg_extention_21s.csv",
                  names = [0,1,2,3,4,5,6,7],
                  encoding= 'cp949')
#ms 단위로 평균값 list에 등록
channel_1to8 = []
for i in range(len(csv.columns)):
    line = []
    for j in range(0, len(csv), 25):
        o = csv[i][j: j + 25]
        line.append(np.mean(o))
    channel_1to8.append(line)

#원하는 지점 입력. 단위는 ms(0.1초 단위)
time_A = int(input("A_value를 입력하세요"))
print(f'A_value {time_A} ms')
time_B = int(input("B_value를 입력하세요"))
print(f'B_value {time_B} ms')

#channel_1부터8까지 원하는 time A, B, A-B 출력
print('         A_value    B_value    A-B_value')
for i in range(len(csv.columns)):
    print(f'channel{i+1} {round(channel_1to8[i][(time_A)],3)}, {round(channel_1to8[i][(time_B)],3)}, {round(channel_1to8[i][(time_A)]-channel_1to8[i][(time_B)],3)}')




