| ?- ['main'].
compiling /home/thomas/Programmation/UVs/ia02/ai-siam/main.pl for byte code...
/home/thomas/Programmation/UVs/ia02/ai-siam/main.pl compiled, 510 lines read - 73416 bytes written, 36 ms

(8 ms) yes
| ?- ai_vs_ai.
+-----+-----+-----+-----+-----+
|     |     |     |     |     |
+-----+-----+-----+-----+-----+
|     |     |     |     |     |
+-----+-----+-----+-----+-----+
|     |  M  |  M  |  M  |     |
+-----+-----+-----+-----+-----+
|     |     |     |     |     |
+-----+-----+-----+-----+-----+
|     |     |     |     |     |
+-----+-----+-----+-----+-----+
[[(0,n),(0,n),(0,w),(0,e),(0,e)],[(0,s),(0,s),(0,s),(0,w),(0,e)],[32,33,34],e]
Player (e) must play
Choose : 0,35,w score : 0
+-----+-----+-----+-----+-----+
|     |     |     |     |     |
+-----+-----+-----+-----+-----+
|     |     |     |     |     |
+-----+-----+-----+-----+-----+
|     |  M  |  M  |  M  | E←  |
+-----+-----+-----+-----+-----+
|     |     |     |     |     |
+-----+-----+-----+-----+-----+
|     |     |     |     |     |
+-----+-----+-----+-----+-----+
[[(35,w),(0,n),(0,w),(0,e),(0,e)],[(0,s),(0,s),(0,s),(0,w),(0,e)],[32,33,34],r]
Player (r) must play
Choose : 0,31,e score : -1
+-----+-----+-----+-----+-----+
|     |     |     |     |     |
+-----+-----+-----+-----+-----+
|     |     |     |     |     |
+-----+-----+-----+-----+-----+
| R→  |  M  |  M  |  M  | E←  |
+-----+-----+-----+-----+-----+
|     |     |     |     |     |
+-----+-----+-----+-----+-----+
|     |     |     |     |     |
+-----+-----+-----+-----+-----+
[[(35,w),(0,n),(0,w),(0,e),(0,e)],[(31,e),(0,s),(0,s),(0,w),(0,e)],[32,33,34],e]
Player (e) must play
Choose : 0,11,n score : -9
+-----+-----+-----+-----+-----+
|     |     |     |     |     |
+-----+-----+-----+-----+-----+
|     |     |     |     |     |
+-----+-----+-----+-----+-----+
| R→  |  M  |  M  |  M  | E←  |
+-----+-----+-----+-----+-----+
|     |     |     |     |     |
+-----+-----+-----+-----+-----+
| E↑  |     |     |     |     |
+-----+-----+-----+-----+-----+
[[(35,w),(11,n),(0,w),(0,e),(0,e)],[(31,e),(0,s),(0,s),(0,w),(0,e)],[32,33,34],r]
Player (r) must play
Choose : 0,12,n score : -1
+-----+-----+-----+-----+-----+
|     |     |     |     |     |
+-----+-----+-----+-----+-----+
|     |     |     |     |     |
+-----+-----+-----+-----+-----+
| R→  |  M  |  M  |  M  | E←  |
+-----+-----+-----+-----+-----+
|     |     |     |     |     |
+-----+-----+-----+-----+-----+
| E↑  | R↑  |     |     |     |
+-----+-----+-----+-----+-----+
[[(35,w),(11,n),(0,w),(0,e),(0,e)],[(31,e),(12,n),(0,s),(0,w),(0,e)],[32,33,34],e]
Player (e) must play
Choose : 0,13,n score : 0
+-----+-----+-----+-----+-----+
|     |     |     |     |     |
+-----+-----+-----+-----+-----+
|     |     |     |     |     |
+-----+-----+-----+-----+-----+
| R→  |  M  |  M  |  M  | E←  |
+-----+-----+-----+-----+-----+
|     |     |     |     |     |
+-----+-----+-----+-----+-----+
| E↑  | R↑  | E↑  |     |     |
+-----+-----+-----+-----+-----+
[[(35,w),(11,n),(13,n),(0,e),(0,e)],[(31,e),(12,n),(0,s),(0,w),(0,e)],[32,33,34],r]
Player (r) must play
Choose : 12,22,n score : -6
+-----+-----+-----+-----+-----+
|     |     |     |     |     |
+-----+-----+-----+-----+-----+
|     |     |     |     |     |
+-----+-----+-----+-----+-----+
| R→  |  M  |  M  |  M  | E←  |
+-----+-----+-----+-----+-----+
|     | R↑  |     |     |     |
+-----+-----+-----+-----+-----+
| E↑  |     | E↑  |     |     |
+-----+-----+-----+-----+-----+
[[(35,w),(11,n),(13,n),(0,e),(0,e)],[(31,e),(22,n),(0,s),(0,w),(0,e)],[32,33,34],e]
Player (e) must play
Choose : 13,23,n score : 0
+-----+-----+-----+-----+-----+
|     |     |     |     |     |
+-----+-----+-----+-----+-----+
|     |     |     |     |     |
+-----+-----+-----+-----+-----+
| R→  |  M  |  M  |  M  | E←  |
+-----+-----+-----+-----+-----+
|     | R↑  | E↑  |     |     |
+-----+-----+-----+-----+-----+
| E↑  |     |     |     |     |
+-----+-----+-----+-----+-----+
[[(35,w),(11,n),(23,n),(0,e),(0,e)],[(31,e),(22,n),(0,s),(0,w),(0,e)],[32,33,34],r]
Player (r) must play
Choose : 0,12,n score : -6
+-----+-----+-----+-----+-----+
|     |     |     |     |     |
+-----+-----+-----+-----+-----+
|     |     |     |     |     |
+-----+-----+-----+-----+-----+
| R→  |  M  |  M  |  M  | E←  |
+-----+-----+-----+-----+-----+
|     | R↑  | E↑  |     |     |
+-----+-----+-----+-----+-----+
| E↑  | R↑  |     |     |     |
+-----+-----+-----+-----+-----+
[[(35,w),(11,n),(23,n),(0,e),(0,e)],[(31,e),(22,n),(12,n),(0,w),(0,e)],[32,33,34],e]
Player (e) must play
Choose : 23,33,n score : -5
+-----+-----+-----+-----+-----+
|     |     |     |     |     |
+-----+-----+-----+-----+-----+
|     |     |  M  |     |     |
+-----+-----+-----+-----+-----+
| R→  |  M  | E↑  |  M  | E←  |
+-----+-----+-----+-----+-----+
|     | R↑  |     |     |     |
+-----+-----+-----+-----+-----+
| E↑  | R↑  |     |     |     |
+-----+-----+-----+-----+-----+
[[(35,w),(11,n),(33,n),(0,e),(0,e)],[(31,e),(22,n),(12,n),(0,w),(0,e)],[32,43,34],r]
Player (r) must play
Choose : 0,53,s score : 4
+-----+-----+-----+-----+-----+
|     |     | R↓  |     |     |
+-----+-----+-----+-----+-----+
|     |     |  M  |     |     |
+-----+-----+-----+-----+-----+
| R→  |  M  | E↑  |  M  | E←  |
+-----+-----+-----+-----+-----+
|     | R↑  |     |     |     |
+-----+-----+-----+-----+-----+
| E↑  | R↑  |     |     |     |
+-----+-----+-----+-----+-----+
[[(35,w),(11,n),(33,n),(0,e),(0,e)],[(31,e),(22,n),(12,n),(53,s),(0,e)],[32,43,34],e]
Player (e) must play
Choose : 0,13,e score : -5
+-----+-----+-----+-----+-----+
|     |     | R↓  |     |     |
+-----+-----+-----+-----+-----+
|     |     |  M  |     |     |
+-----+-----+-----+-----+-----+
| R→  |  M  | E↑  |  M  | E←  |
+-----+-----+-----+-----+-----+
|     | R↑  |     |     |     |
+-----+-----+-----+-----+-----+
| E↑  | R↑  | E→  |     |     |
+-----+-----+-----+-----+-----+
[[(35,w),(11,n),(33,n),(13,e),(0,e)],[(31,e),(22,n),(12,n),(53,s),(0,e)],[32,43,34],r]
Player (r) must play
Choose : 0,14,n score : 4
+-----+-----+-----+-----+-----+
|     |     | R↓  |     |     |
+-----+-----+-----+-----+-----+
|     |     |  M  |     |     |
+-----+-----+-----+-----+-----+
| R→  |  M  | E↑  |  M  | E←  |
+-----+-----+-----+-----+-----+
|     | R↑  |     |     |     |
+-----+-----+-----+-----+-----+
| E↑  | R↑  | E→  | R↑  |     |
+-----+-----+-----+-----+-----+
[[(35,w),(11,n),(33,n),(13,e),(0,e)],[(31,e),(22,n),(12,n),(53,s),(14,n)],[32,43,34],e]
Player (e) must play
Choose : 13,14,e score : -17
+-----+-----+-----+-----+-----+
|     |     | R↓  |     |     |
+-----+-----+-----+-----+-----+
|     |     |  M  |     |     |
+-----+-----+-----+-----+-----+
| R→  |  M  | E↑  |  M  | E←  |
+-----+-----+-----+-----+-----+
|     | R↑  |     |     |     |
+-----+-----+-----+-----+-----+
| E↑  | R↑  |     | E→  | R↑  |
+-----+-----+-----+-----+-----+
[[(35,w),(11,n),(33,n),(14,e),(0,e)],[(31,e),(22,n),(12,n),(53,s),(15,n)],[32,43,34],r]
Player (r) must play
Choose : 15,25,n score : 14
+-----+-----+-----+-----+-----+
|     |     | R↓  |     |     |
+-----+-----+-----+-----+-----+
|     |     |  M  |     |     |
+-----+-----+-----+-----+-----+
| R→  |  M  | E↑  |  M  | E←  |
+-----+-----+-----+-----+-----+
|     | R↑  |     |     | R↑  |
+-----+-----+-----+-----+-----+
| E↑  | R↑  |     | E→  |     |
+-----+-----+-----+-----+-----+
[[(35,w),(11,n),(33,n),(14,e),(0,e)],[(31,e),(22,n),(12,n),(53,s),(25,n)],[32,43,34],e]
Player (e) must play
Choose : 0,13,n score : -27
+-----+-----+-----+-----+-----+
|     |     | R↓  |     |     |
+-----+-----+-----+-----+-----+
|     |     |  M  |     |     |
+-----+-----+-----+-----+-----+
| R→  |  M  | E↑  |  M  | E←  |
+-----+-----+-----+-----+-----+
|     | R↑  |     |     | R↑  |
+-----+-----+-----+-----+-----+
| E↑  | R↑  | E↑  | E→  |     |
+-----+-----+-----+-----+-----+
[[(35,w),(11,n),(33,n),(14,e),(13,n)],[(31,e),(22,n),(12,n),(53,s),(25,n)],[32,43,34],r]
Player (r) must play
Choose : 25,24,n score : 14
+-----+-----+-----+-----+-----+
|     |     | R↓  |     |     |
+-----+-----+-----+-----+-----+
|     |     |  M  |     |     |
+-----+-----+-----+-----+-----+
| R→  |  M  | E↑  |  M  | E←  |
+-----+-----+-----+-----+-----+
|     | R↑  |     | R↑  |     |
+-----+-----+-----+-----+-----+
| E↑  | R↑  | E↑  | E→  |     |
+-----+-----+-----+-----+-----+
[[(35,w),(11,n),(33,n),(14,e),(13,n)],[(31,e),(22,n),(12,n),(53,s),(24,n)],[32,43,34],e]
Player (e) must play
Choose : 13,23,n score : -27
+-----+-----+-----+-----+-----+
|     |     | R↓  |     |     |
+-----+-----+-----+-----+-----+
|     |     |  M  |     |     |
+-----+-----+-----+-----+-----+
| R→  |  M  | E↑  |  M  | E←  |
+-----+-----+-----+-----+-----+
|     | R↑  | E↑  | R↑  |     |
+-----+-----+-----+-----+-----+
| E↑  | R↑  |     | E→  |     |
+-----+-----+-----+-----+-----+
[[(35,w),(11,n),(33,n),(14,e),(23,n)],[(31,e),(22,n),(12,n),(53,s),(24,n)],[32,43,34],r]
Player (r) must play
Choose : _1381 score : -999
+-----+-----+-----+-----+-----+
|     |     | R↓  |     |     |
+-----+-----+-----+-----+-----+
|     |     |  M  |     |     |
+-----+-----+-----+-----+-----+
| R→  |  M  | E↑  |  M  | E↑  |
+-----+-----+-----+-----+-----+
|     | R↑  | E↑  | R↑  |     |
+-----+-----+-----+-----+-----+
| E↑  | R↑  |     | E→  |     |
+-----+-----+-----+-----+-----+
[[(35,n),(11,n),(33,n),(14,e),(23,n)],[(31,e),(22,n),(12,n),(53,s),(24,n)],[32,43,34],e]
Player (e) must play
Choose : 23,33,n score : -3
+-----+-----+-----+-----+-----+
|     |     |  M  |     |     |
+-----+-----+-----+-----+-----+
|     |     | E↑  |     |     |
+-----+-----+-----+-----+-----+
| R→  |  M  | E↑  |  M  | E←  |
+-----+-----+-----+-----+-----+
|     | R↑  |     | R↑  |     |
+-----+-----+-----+-----+-----+
| E↑  | R↑  |     | E→  |     |
+-----+-----+-----+-----+-----+
[[(35,w),(11,n),(43,n),(14,e),(33,n)],[(31,e),(22,n),(12,n),(0,s),(24,n)],[32,53,34],r]
Player (r) must play
Choose : _2183 score : -999
+-----+-----+-----+-----+-----+
|     |     |  M  |     |     |
+-----+-----+-----+-----+-----+
|     |     | E↑  |     |     |
+-----+-----+-----+-----+-----+
| R→  |  M  | E↑  |  M  | E↑  |
+-----+-----+-----+-----+-----+
|     | R↑  |     | R↑  |     |
+-----+-----+-----+-----+-----+
| E↑  | R↑  |     | E→  |     |
+-----+-----+-----+-----+-----+
[[(35,n),(11,n),(43,n),(14,e),(33,n)],[(31,e),(22,n),(12,n),(0,s),(24,n)],[32,53,34],e]
Player (e) must play
Choose : 33,43,n score : 999
gagnant : e

(31602 ms) yes

