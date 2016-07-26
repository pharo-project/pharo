rewrite code
rewriteCodeText
	^ 'RBParseTreeRewriter new
	replace: ''`@object'' with: ''`@object'';
	replace: ''`@object'' with: ''`@object'' when: [ :node | true ];
	replace: ''`@object'' withValueFrom: [ :node | node ];
	replace: ''`@object'' withValueFrom: [ :node | node ] when: [ :node | true ];
	replaceMethod: ''`@method: `@args | `@temps | `@.statements'' with: ''`@method: `@args | `@temps | `@.statements'';
	replaceMethod: ''`@method: `@args | `@temps | `@.statements'' with: ''`@method: `@args | `@temps | `@.statements'' when: [ :node | true ];
	replaceMethod: ''`@method: `@args | `@temps | `@.statements'' withValueFrom: [ :node | node ];
	replaceMethod: ''`@method: `@args | `@temps | `@.statements'' withValueFrom: [ :node | node ] when: [ :node | true ];
	yourself'