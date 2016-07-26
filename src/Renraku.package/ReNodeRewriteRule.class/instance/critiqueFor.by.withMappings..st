helpers
critiqueFor: aNode by: rewriter withMappings: map


	^ ReReplaceNodeCritique
		rule: self
		achor: (self anchorFor: aNode)
		oldNode: aNode
		newNode: (rewriter rewriten: aNode with: map)