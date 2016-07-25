*TxText-Athens
newCharacterSpanDisplayCommand: span start: startPos stop: stopPos advance: aPoint

	^ TxCairoCharacterSpanDisplayCommand new 
			span: span start: startPos stop: stopPos;
			advance: aPoint;
			yourself