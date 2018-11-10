Extension { #name : #ComposablePresenter }

{ #category : #'*Iceberg-TipUI' }
ComposablePresenter class >> iceActionPanelHeight [
	^ IceTipActionPanel defaultHeight
]

{ #category : #'*Iceberg-TipUI' }
ComposablePresenter class >> iceIconWidth [
	^ 24 * World displayScaleFactor
]

{ #category : #'*Iceberg-TipUI' }
ComposablePresenter class >> iceLabelWidth [
	^ 100 * World displayScaleFactor
]

{ #category : #'*Iceberg-TipUI' }
ComposablePresenter >> newIceActionPanel [
	^ self instantiate: IceTipActionPanel
]

{ #category : #'*Iceberg-TipUI' }
ComposablePresenter >> newIceOutline [
	self flag: #todo. "This should be a spec component (but we don't have it for the moment)"
	^ IceTipOutlineMorph new
		hResizing: #spaceFill;
		vResizing: #spaceFill;
		asSpecAdapter
]

{ #category : #'*Iceberg-TipUI' }
ComposablePresenter >> newIcePanel [
	^ PanelMorph new 
		changeTableLayout;
		hResizing: #spaceFill;
		vResizing: #spaceFill;
		asSpecAdapter
]

{ #category : #'*Iceberg-TipUI' }
ComposablePresenter >> newIceReadOnlyText [ 
	^ IceTipReadOnlyTextMorph new
		hResizing: #spaceFill;
		vResizing: #spaceFill;
		asSpecAdapter
]

{ #category : #'*Iceberg-TipUI' }
ComposablePresenter >> newIceTable [
	self flag: #todo. "This should be a spec component (but we don't have it for the moment)"
	^ FTTableMorph new
		hResizing: #spaceFill;
		vResizing: #spaceFill;
		asSpecAdapter
]

{ #category : #'*Iceberg-TipUI' }
ComposablePresenter >> newIceToolbar [
	^ self instantiate: IceTipToolbar
]

{ #category : #'*Iceberg-TipUI' }
ComposablePresenter >> newIceTreeTable [
	"For the moment, this is the same as a table, but it will receive a tree datasource.
	 But this needs to be a spec component"
	self flag: #todo. "This should be a spec component (but we don't have it for the moment)"
	^ FTTableMorph new
		hResizing: #spaceFill;
		vResizing: #spaceFill;
		asSpecAdapter
]
