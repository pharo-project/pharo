private
spanNewSessionForContext: aContext fromProcess: aProcess

	^ self sessionClass 
		named: 'temporary name' on: aProcess startedAt: aContext