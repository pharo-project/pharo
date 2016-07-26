class analysis
compareTallyIn: beforeFileName to: afterFileName
	"SpaceTally new compareTallyIn: 'tally' to: 'tally2'"

	| answer beforeDict a afterDict allKeys |
	beforeDict := Dictionary new.
	beforeFileName asFileReference readStreamDo: [ :s|
		[s atEnd] whileFalse: [
			a := Array readFrom: s nextLine.
			beforeDict at: a first put: a allButFirst ]].
	afterDict := Dictionary new.
	
	afterFileName asFileReference readStreamDo: [ :s|
		[s atEnd] whileFalse: [
			a := Array readFrom: s nextLine.
			afterDict at: a first put: a allButFirst ]].

	answer := String new writeStream.
	allKeys := (Set new addAll: beforeDict keys; addAll: afterDict keys; yourself) asSortedCollection.
	allKeys do: [ :each | | before diff after |
		before := beforeDict at: each ifAbsent: [#(0 0 0)].
		after := afterDict at: each ifAbsent: [#(0 0 0)].
		diff := before with: after collect: [ :vBefore :vAfter | vAfter - vBefore].
		diff = #(0 0 0) ifFalse: [
			answer nextPutAll: each,'  ',diff printString; cr.
		].
	].
	StandardWindow new
		withText: answer contents
		label: 'space diffs';
		openInWorld
	


