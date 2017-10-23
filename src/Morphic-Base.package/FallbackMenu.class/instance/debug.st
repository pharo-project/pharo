menu actions
debug

	"If we don't reset the builder after debugging, the fallback menu does not appear anymore"
	[ builder menuSpec asMenuMorph ] ensure: [ builder reset ].