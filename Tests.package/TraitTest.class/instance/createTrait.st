utils
createTrait
	| trait |
	trait := Trait named: #ATraitForTests uses: {} category: 'AAA'.
	trait compile: 'm1 ^1' classified: 'test'.
	^ trait