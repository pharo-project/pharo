serialize/materialize
materializeReferencesOf: aRectangle with: aDecoder
		
		aRectangle 
			fuelSetOrigin: aDecoder nextEncodedReference @ aDecoder nextEncodedReference	
		 	corner: aDecoder nextEncodedReference @ aDecoder nextEncodedReference