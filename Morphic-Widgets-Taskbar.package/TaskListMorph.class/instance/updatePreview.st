private
updatePreview
	"Update the preview.."
	
	self preview removeAllMorphs.
	self preview addMorphCentered: ( 
		self activeTask 
				ifNil: [ 'No tasks...' asMorph	]
				ifNotNil: [:t | t morph taskThumbnailOfSize: self preview extent])