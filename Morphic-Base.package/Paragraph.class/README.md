A Paragraph represents text that has been laid out, or composed, in some container.
I also display the different kinds of text selection (secondary, find-replace and selection bar).

Class collaborations
   SelectionBlock instances are built by myself and stored in the extraSelectionBlocks instance variable in order to display supplementary selections

Class main API
   no particular main API.

Instance Variables
   extraSelectionBlocks: <Collection of SelectionBlock>
   findReplaceSelectionRegex: <RxMatcher>
   presentationLines: <Object>
   presentationText: <Object>
   refreshExtraSelection: <Boolean>
   secondarySelection: <String>

extraSelectionBlocks
   - a collection of SelectionBlock for the drowing of the differents kind of text selection

findReplaceSelectionRegex
   - the find/replace matcher that is set from the editor of by the FindReplaceService

presentationLines
   - created for debugging purpose, should be removed ?

presentationText
   - created for debugging purpose, should be removed ?

refreshExtraSelection
   - a boolean that is set to tru when there is a need to refresh selections

secondarySelection
   - the string of the secondary selection that is set from the editor when a portion of text is selected

	text 		A Text with encoded per-character emphasis.
	textStyle	A TextStyle with font set, line height and horizontal alignment.
	firstCharacterIndex    The starting index in text for this paragraph, allowing
				composition of a long text into a number of containers.
	container	A Rectangle or TextContainer that determines where text can go.
	lines		An Array of TextLines comprising the final layout of the text
				after it has been composed within its container.
	positionWhenComposed   As its name implies.  Allows display at new locations
				without the need to recompose the text.
Lines are ordered vertically.  However, for a given y, there may be several lines in left to right order.  Lines must never be empty, even if text is empty.

Notes on yet another hack - 5 Feb 2001

We really need to clean up #composeLinesFrom:to:delta:into:priorLines:atY:!!!

I added one more habdful of code to correct:

This is an annoying bug that's been around for a couple of years, but I finally figured out how to duplicate the problem, so I figured I'd just report it now.  (It doesn't necessarily have to be fixed for 3.0 if it looks messy, but if it's a simple fix, it would be worth it.)

In Morphic, if you have the following text in a workspace:

This is line 1
This is line 2

**and** you have a return character after line 2, you will normally be able to click the mouse two times below line 2 in order to select all the text.  If you edit line 2 (e.g. so that it reads "line number 2"), you can still select all the text by clicking below the second line.  However, if you edit line 1, you will not be able to select all the text from the bottom in the same way.  Things get messed up such that the last return character seems to be gone.  In this state, if you position the cursor immediately after the 2, and press the right arrow, the cursor jumps to the beginning of line 2... oof. (report by Doug Way)

While I don't have a very deep understanding of the above mentioned method, I was able to determine that text ending in a CR worked better in the editor when the last entry in <lines> had a start of text size + 1 and a stop of text size. I have accordingly added code near the end to ensure this. It seems to have fixed the problem, but we do need to clean this baby up some day. - Bob


