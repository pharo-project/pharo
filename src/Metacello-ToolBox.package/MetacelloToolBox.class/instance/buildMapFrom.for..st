private
buildMapFrom: mapList for: packageList
	| map |
	map := Dictionary new.
	mapList
		do: [ :assoc | 
			| pkgName pkgSpec |
			pkgName := assoc key.
			(packageList includes: pkgName)
				ifFalse: [ self error: 'package ' , pkgName printString , ' not found in packages.' ].
			map at: pkgName put: assoc value ].
	^ map