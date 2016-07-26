tutorial metainfo
title	
	"Return the title of the tutorial by parsing the class name 	
	like a Wiki link and interspersing whitespaces between the tokens"	
	| className separators groups |	
			
	className := self name.	
	separators := 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'.	
	groups := className findTokens: separators keep: separators.	
	^' ' join: (groups pairsCollect: [ :sep :rest | sep , rest ]).
