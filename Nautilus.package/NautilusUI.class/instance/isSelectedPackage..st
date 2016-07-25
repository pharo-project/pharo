private
isSelectedPackage: aSelection 
         aSelection 
      ifNil:[^ self selectedPackage isNil]
	ifNotNil:[^ aSelection asNautilusSelection = self selectedPackage ]	