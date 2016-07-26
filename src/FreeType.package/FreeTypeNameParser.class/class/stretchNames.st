known names
stretchNames
	"Answer a sequence of arrays.
	Each array has an integer stretch value as its first element (1 - 9).
	The remaining elements are String tokens which might appear
	within a font family-style name"
	"
	TO RE-INITIALIZE...
	self instVarNamed: #stretchNames put: nil.
	"
	stretchNames ifNotNil:[^stretchNames].
	^stretchNames := {
		{LogicalFont stretchExtraCompressed. 'extra'. 'compressed'}.
		{LogicalFont stretchExtraCompressed. 'extracompressed'}.
		{LogicalFont stretchExtraCompressed. 'ext'. 'compressed'}.
		{LogicalFont stretchExtraCompressed. 'extcompressed'}.
		{LogicalFont stretchUltraCompressed. 'ultra'. 'compressed'}.
		{LogicalFont stretchUltraCompressed. 'ultracompressed'}.
		{LogicalFont stretchUltraCondensed. 'ultra'. 'condensed'}.
		{LogicalFont stretchUltraCondensed. 'ultracondensed'}.
		{LogicalFont stretchUltraCondensed. 'ultra'. 'cond'}.
		{LogicalFont stretchUltraCondensed. 'ultracond'}.
		{LogicalFont stretchCompressed. 'compressed'}.
		{LogicalFont stretchExtraCondensed. 'extra'. 'condensed'}.
		{LogicalFont stretchExtraCondensed. 'extracondensed'}.
		{LogicalFont stretchExtraCondensed. 'ext'. 'condensed'}.
		{LogicalFont stretchExtraCondensed. 'extcondensed'}.
		{LogicalFont stretchExtraCondensed. 'extra'. 'cond'}.
		{LogicalFont stretchExtraCondensed. 'extracond'}.
		{LogicalFont stretchExtraCondensed. 'ext'. 'cond'}.
		{LogicalFont stretchExtraCondensed. 'extcond'}.
		{LogicalFont stretchNarrow. 'narrow'}.
		{LogicalFont stretchCompact. 'compact'}.
		{LogicalFont stretchSemiCondensed. 'semi'. 'condensed'}.
		{LogicalFont stretchSemiCondensed. 'semicondensed'}.
		{LogicalFont stretchSemiCondensed. 'semi'. 'cond'}.
		{LogicalFont stretchSemiCondensed. 'semicond'}.
		{LogicalFont stretchWide. 'wide'}.
		{LogicalFont stretchSemiExpanded. 'semi'. 'expanded'}.
		{LogicalFont stretchSemiExpanded. 'semiexpanded'}.
		{LogicalFont stretchSemiExtended. 'semi'. 'extended'}.
		{LogicalFont stretchSemiExtended. 'semiextended'}.
		{LogicalFont stretchExtraExpanded. 'extra'. 'expanded'}.
		{LogicalFont stretchExtraExpanded. 'extraexpanded'}.
		{LogicalFont stretchExtraExpanded. 'ext'. 'expanded'}.
		{LogicalFont stretchExtraExpanded. 'extexpanded'}.
		{LogicalFont stretchExtraExtended. 'extra'. 'extended'}.
		{LogicalFont stretchExtraExtended. 'extraextended'}.
		{LogicalFont stretchExtraExtended. 'ext'. 'extended'}.
		{LogicalFont stretchExtraExtended. 'extextended'}.
		{LogicalFont stretchUltraExpanded. 'ultra'. 'expanded'}.
		{LogicalFont stretchUltraExpanded. 'ultraexpanded'}.
		{LogicalFont stretchUltraExtended. 'ultra'. 'extended'}.
		{LogicalFont stretchUltraExtended. 'ultraextended'}.
		{LogicalFont stretchCondensed. 'condensed'}.
		{LogicalFont stretchCondensed. 'cond'}.
		{LogicalFont stretchExpanded. 'expanded'}.
		{LogicalFont stretchExtended. 'extended'}	
		}.  "search for them in the order given here"
