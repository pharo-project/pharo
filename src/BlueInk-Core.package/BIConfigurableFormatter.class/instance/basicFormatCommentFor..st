private
basicFormatCommentFor: aComment
	codeStream nextPutAll: (originalSource copyFrom: aComment start to: aComment stop).