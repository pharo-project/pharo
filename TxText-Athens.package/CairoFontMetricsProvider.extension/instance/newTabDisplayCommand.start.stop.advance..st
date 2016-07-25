*TxText-Athens
newTabDisplayCommand: span start: startPos stop: stopPos advance: aPoint 

	^ TxTabDisplayCommand new 
		span: span start: startPos stop: stopPos;
		advance: aPoint;
		yourself