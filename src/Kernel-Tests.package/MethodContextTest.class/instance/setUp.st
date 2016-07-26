running
setUp
	super setUp.
	aCompiledMethod := Rectangle methodDict at: #rightCenter.
	aReceiver := 100@100 corner: 200@200.
	aSender := thisContext.
	aMethodContext := Context sender: aSender receiver: aReceiver method: aCompiledMethod arguments: #(). 