printing
rootPrintOn: aStream total: total totalTime: totalTime threshold: threshold

	| groups |
	groups := (self sonsOver: threshold) groupedBy: [ :aTally | aTally process] having: [ :g | true ].
	groups do: [ :g |
		| sons p |
		sons := g asArray sort.
		p := g anyOne process.
		(reportOtherProcesses or: [ p notNil ]) ifTrue: [
			aStream nextPutAll: '--------------------------------'; cr.
			aStream nextPutAll: 'Process: ',  (p ifNil: [ 'other processes'] ifNotNil: [ p browserPrintString]); cr.
			aStream nextPutAll: '--------------------------------'; cr.
			sons do: [ :son |
				son
					treePrintOn: aStream
					tabs: OrderedCollection new
					thisTab: ''
					total: total
					totalTime: totalTime
					tallyExact: false
					orThreshold: threshold]].
	]