"Evaluate me to view the SplitJoin documentation:"

self showDocumentation

"CHANGES LOG:
- merged implementations by Keith Hodges (Join) and Damiena Pollet
  and Oscar Nierstrasz (RubyShards) into SplitJoin package
- moved all extension methods to *splitjoin method category
- merged all tests into SplitJoinTest
- fixed protocol in SequenceableCollection to splitOn: and joinUsing:
  and split: join: for splitters and joiners
- added Object>>joinTo: aStream and SequenceableCollection>>joinTo: aStream
  to support joining of either sequences or sequences of sequences
- prepared some documentation
- added systematic tests for all split/join cases
- added Object>>join:
- prepared split/join tests for all 16 cases
- prepares split+join tests for 4 standard cases
- reviewed/merged old tests
- changed splitjoin tests to use different joiner
- added separate test for split+join=id cases
- adapted documentation -- join result type is type of joiner or array or string
- fix split tests to check result asOrderedCollection
- added split tests for OrderedCollection and SortedCollection
- new join: method for OrderedCollection and SortedCollection
  (uses appendTo: in Object and SequenceableCollection)
- reviewed all split: implementations -- removed unnecessary helper methods
- check boundary conditions -- split on empty sequence ...
"