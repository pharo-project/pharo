private
isValidMczName: string
	^ string isString 
		and: [ string isEmpty not
			and: [ (string endsWith: '.mcz')
				and: [ (string includes: $/) not ] ] ]