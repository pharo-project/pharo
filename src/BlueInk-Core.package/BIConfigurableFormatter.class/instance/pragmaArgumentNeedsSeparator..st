private
pragmaArgumentNeedsSeparator: anArgumentNode
	^ anArgumentNode value isSymbol and: [ anArgumentNode value isBinary ]