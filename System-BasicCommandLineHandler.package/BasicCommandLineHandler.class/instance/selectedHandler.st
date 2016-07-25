private
selectedHandler
	^ self selectHandlers 
		ifEmpty: [ nil ] 
		ifNotEmpty: [ :handlers | handlers first ]