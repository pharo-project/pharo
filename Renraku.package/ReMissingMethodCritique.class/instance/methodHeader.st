private
methodHeader

	 ^ ($: split: selector) allButLast
		ifEmpty: [ selector ]
		ifNotEmpty: [ :keywords |
			| index |
			index := 0.
			keywords inject: '' into: [ :header :keyword |
				header, keyword, ': arg', (index := index + 1) asString, ' ' ] ]