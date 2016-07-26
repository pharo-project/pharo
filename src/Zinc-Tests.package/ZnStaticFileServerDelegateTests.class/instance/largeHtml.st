private
largeHtml
	^ String streamContents: [ :out |
		out << '<html><body>'.
		out << 'Les élèves Français'.
		(1 to: 4000) do: [ :each | out print: each ] separatedBy: [ out space ]. 
		out << '</html></body>' ]