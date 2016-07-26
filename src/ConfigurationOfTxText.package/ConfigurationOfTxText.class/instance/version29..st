versions
version29: spec
	<version: '2.9' imports: #('1.0.1-baseline' )>

	spec for: #'common' do: [
		spec blessing: #'stable'.
		spec description: 'Issue 15481: TxText Cleanup for Pharo 5.0 #3
https://pharo.fogbugz.com/default.asp?15481

### This version:
[Fix validator errors](https://ci.inria.fr/pharo/job/Pharo-5.0-Issue-Validator/1233/artifact/validationReport.html)
Nuke last usages of TxModel>>#getAttribute:
- Spans - if they don''t have a requested attribute, get it from the text''s default style
- Update tests

### Previous version
- [Clean]: many comments, typo fixes, etc
- [ENH]: Remove TxModel>>#getAttribute:, which makes no sense - you must ask a span or position for an attribute, not a text
- [ENH]: Rename TxModel>>#import: -> #replaceAllWith:'.
		spec author: 'SeanDeNigris'.
		spec timestamp: '5/4/2015 01:38'.
		spec 
			package: 'TxText-Model' with: 'TxText-Model-SeanDeNigris.59';
			package: 'TxTextTests-Model' with: 'TxTextTests-Model-SeanDeNigris.28';
			package: 'TxText-Athens' with: 'TxText-Athens-SeanDeNigris.49';
			package: 'TxText-AthensTests' with: 'TxText-AthensTests-SeanDeNigris.10';
			package: 'TxText-Styler' with: 'TxText-Styler-SeanDeNigris.5';
			package: 'Tool-TxWorkspace' with: 'Tool-TxWorkspace-TorstenBergmann.5'. ].