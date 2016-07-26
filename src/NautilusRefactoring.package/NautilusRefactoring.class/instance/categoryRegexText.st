rewrite code
categoryRegexText
	^ 'RBCategoryRegexRefactoring new
	replace: ''^Kernel-(.*)$'' with: ''System-$1'' ignoreCase: false;
	yourself'