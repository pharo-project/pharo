versions
version285: spec
	<version: '2.8.5' imports: #('1.0.1-baseline' )>

	spec for: #'common' do: [
		spec blessing: #'stable'.
		spec description: 'Issue 15475: TxText Cleanup for Pharo 5.0 #2
- TxXyzMorphs
	- Push up #openInWindowWithString:, #openInWindowWithText:, and #text, which each had two materially-identical implementations
	- Clean examples
- TxModel - enhance class comment
- TxStyle
	- Enhance class comment
	- Clean #removeAll comment
- TxTextStyle
	- Enhance class comment
	- Clean #addStyle: comment
	- Clean #extend:with: comment
- Apply some Lint Rules found while fixing validation warnings
	- https://ci.inria.fr/pharo/job/Pharo-5.0-Issue-Validator/1141//artifact/validationReport.html
	- https://ci.inria.fr/pharo/job/Pharo-5.0-Issue-Validator/1174//artifact/validationReport.html
	- https://ci.inria.fr/pharo/job/Pharo-5.0-Issue-Validator/1179//artifact/validationReport.html'.
		spec author: 'SeanDeNigris'.
		spec timestamp: '5/3/2015 13:38'.
		spec 
			package: 'TxText-Model' with: 'TxText-Model-SeanDeNigris.57';
			package: 'TxTextTests-Model' with: 'TxTextTests-Model-IgorStasenko.26';
			package: 'TxText-Athens' with: 'TxText-Athens-SeanDeNigris.48';
			package: 'TxText-AthensTests' with: 'TxText-AthensTests-IgorStasenko.9';
			package: 'TxText-Styler' with: 'TxText-Styler-SeanDeNigris.5';
			package: 'Tool-TxWorkspace' with: 'Tool-TxWorkspace-TorstenBergmann.5'. ].