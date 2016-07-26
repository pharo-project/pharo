results
beforeHooks
	^links 
			select: [ :each | each control = #before ] 
			thenCollect:  [ :link | (self hookFor: link) parent: node] 