initialization
initialize
	super initialize.
	self
		replace: '[| `@temps | ``@.Statements. `var := ``@object] ensure: ``@block'
			with: '`var := [| `@temps | ``@.Statements. ``@object] ensure: ``@block';
		replace: '[| `@temps | ``@.Statements. ^``@object] ensure: ``@block'
			with: '^[| `@temps | ``@.Statements. ``@object] ensure: ``@block';
		replace:'[| `@temps | ``@.Statements. `var := ``@object] ifCurtailed: ``@block'
			with: '`var := [| `@temps | ``@.Statements. ``@object] ifCurtailed: ``@block';
		replace:'[| `@temps | ``@.Statements. ^``@object] ifCurtailed: ``@block'
			with: '^[| `@temps | ``@.Statements. ``@object] ifCurtailed: ``@block'