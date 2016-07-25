versions
version210: spec
	<version: '2.10' imports: #('1.0.1-baseline' )>

	spec for: #'common' do: [
		spec blessing: #'stable'.
		spec description: 'Issue 15567 - TxText Cleanup for Pharo 5.0 #4
https://pharo.fogbugz.com/default.asp?15567
		
### This version:
- Update tests which were failing due to #selection -> #newSelection switch

### v. 1
#### Critics Browser - another pass, fix a bunch of rule violations
#### TxModel
- Move factory methods into a suitable protocol
    - Rename #cursor to #newCursor to be clear that it''s not *the* cursor, but *a* cursor
    - Simplify selecting protocol
        - Only keep #newSelection, which captures the concept the best because this is a different concept than what we think of as "select all" i.e. an editor has only-one selection; here we can have any number of selections which live independently of the text
        - Remove other methods, which all did the same thing as the above
- Remove unsent private methods

#### Other
- [ENH]: Clarify TxTextCursor comment'.
		spec author: 'SeanDeNigris'.
		spec timestamp: '5/21/2015 01:38'.
		spec 
			package: 'TxText-Model' with: 'TxText-Model-SeanDeNigris.61';
			package: 'TxTextTests-Model' with: 'TxTextTests-Model-SeanDeNigris.29';
			package: 'TxText-Athens' with: 'TxText-Athens-SeanDeNigris.50';
			package: 'TxText-AthensTests' with: 'TxText-AthensTests-SeanDeNigris.10';
			package: 'TxText-Styler' with: 'TxText-Styler-SeanDeNigris.6';
			package: 'Tool-TxWorkspace' with: 'Tool-TxWorkspace-TorstenBergmann.5'. ].