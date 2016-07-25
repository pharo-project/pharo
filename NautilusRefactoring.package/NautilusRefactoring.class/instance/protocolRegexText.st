rewrite code
protocolRegexText
	^ 'RBProtocolRegexRefactoring new
	replace: ''^\*system(.*)$'' with: ''*kernel$1'' ignoreCase: true;
	yourself'