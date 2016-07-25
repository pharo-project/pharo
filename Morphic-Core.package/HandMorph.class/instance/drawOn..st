drawing
drawOn: aCanvas 
	"Draw the hand itself (i.e., the cursor)."

	temporaryCursor 
		ifNil: [aCanvas paintImage: NormalCursor at: bounds topLeft]
		ifNotNil: [aCanvas paintImage: temporaryCursor at: bounds topLeft].
	