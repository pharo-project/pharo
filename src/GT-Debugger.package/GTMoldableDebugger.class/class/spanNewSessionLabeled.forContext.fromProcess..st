private
spanNewSessionLabeled: aString forContext: aContext fromProcess: aProcess

	^ self sessionClass 
		named: aString on: aProcess startedAt: aContext