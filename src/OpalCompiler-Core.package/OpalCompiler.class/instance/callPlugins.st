private
callPlugins
	| plugins  |
	plugins := compilationContext astTransformPlugins ifEmpty: [ ^self ].
	plugins sort: [:a :b | a priority > b priority]. "priority 0 is sorted last"
	ast := ast copy.
	plugins do: [ :each | ast := each transform: ast]. 