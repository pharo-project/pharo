spec
defaultSpec
	"Define the layout of the Release Panel"

	<spec>
	^ SpecLayout composed
		newColumn: [ :c | 
					c
						newRow: [ :r1 | 
									r1
										add: #releaseNumberLabel origin: 0@0 corner: 1@1;
										add: #releaseNumber origin: 0@40 corner: 1@100]
							height: self inputTextHeight;
						newRow: [ :r2 | 
									r2 add: #symbolicVersionResolutionCheckBox ]
							height: self inputTextHeight ];
		yourself