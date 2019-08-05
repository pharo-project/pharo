"
GIF Specific ReadWriter tests. All tests ensure that the images written are read back in correctly and visa-versa. Using different colors helps ensure that the resulting mapped pixel data for each is reasonably correct.
"
Class {
	#name : #GIFReadWriterTest,
	#superclass : #TestCase,
	#category : #'Graphics-Tests-Files'
}

{ #category : #accessing }
GIFReadWriterTest class >> animGifExampleBytes [
	"Complete Bytes of the anim example gif file"
	^ #[71 73 70 56 57 97 42 0 42 0 231 0 0 255 255 255 0 0 0 255 255 255 128 128 128 255 0 0 0 255 0 0 0 255 0 255 255 255 255 0 255 0 255 32 32 32 64 64 64 96 96 96 159 159 159 191 191 191 223 223 223 8 8 8 16 16 16 24 24 24 40 40 40 48 48 48 56 56 56 72 72 72 80 80 80 88 88 88 104 104 104 112 112 112 120 120 120 135 135 135 143 143 143 151 151 151 167 167 167 175 175 175 183 183 183 199 199 199 207 207 207 215 215 215 231 231 231 239 239 239 247 247 247 0 0 0 0 51 0 0 102 0 0 153 0 0 204 0 0 255 0 0 0 51 0 51 51 0 102 51 0 153 51 0 204 51 0 255 51 0 0 102 0 51 102 0 102 102 0 153 102 0 204 102 0 255 102 0 0 153 0 51 153 0 102 153 0 153 153 0 204 153 0 255 153 0 0 204 0 51 204 0 102 204 0 153 204 0 204 204 0 255 204 0 0 255 0 51 255 0 102 255 0 153 255 0 204 255 0 255 255 51 0 0 51 51 0 51 102 0 51 153 0 51 204 0 51 255 0 51 0 51 51 51 51 51 102 51 51 153 51 51 204 51 51 255 51 51 0 102 51 51 102 51 102 102 51 153 102 51 204 102 51 255 102 51 0 153 51 51 153 51 102 153 51 153 153 51 204 153 51 255 153 51 0 204 51 51 204 51 102 204 51 153 204 51 204 204 51 255 204 51 0 255 51 51 255 51 102 255 51 153 255 51 204 255 51 255 255 102 0 0 102 51 0 102 102 0 102 153 0 102 204 0 102 255 0 102 0 51 102 51 51 102 102 51 102 153 51 102 204 51 102 255 51 102 0 102 102 51 102 102 102 102 102 153 102 102 204 102 102 255 102 102 0 153 102 51 153 102 102 153 102 153 153 102 204 153 102 255 153 102 0 204 102 51 204 102 102 204 102 153 204 102 204 204 102 255 204 102 0 255 102 51 255 102 102 255 102 153 255 102 204 255 102 255 255 153 0 0 153 51 0 153 102 0 153 153 0 153 204 0 153 255 0 153 0 51 153 51 51 153 102 51 153 153 51 153 204 51 153 255 51 153 0 102 153 51 102 153 102 102 153 153 102 153 204 102 153 255 102 153 0 153 153 51 153 153 102 153 153 153 153 153 204 153 153 255 153 153 0 204 153 51 204 153 102 204 153 153 204 153 204 204 153 255 204 153 0 255 153 51 255 153 102 255 153 153 255 153 204 255 153 255 255 204 0 0 204 51 0 204 102 0 204 153 0 204 204 0 204 255 0 204 0 51 204 51 51 204 102 51 204 153 51 204 204 51 204 255 51 204 0 102 204 51 102 204 102 102 204 153 102 204 204 102 204 255 102 204 0 153 204 51 153 204 102 153 204 153 153 204 204 153 204 255 153 204 0 204 204 51 204 204 102 204 204 153 204 204 204 204 204 255 204 204 0 255 204 51 255 204 102 255 204 153 255 204 204 255 204 255 255 255 0 0 255 51 0 255 102 0 255 153 0 255 204 0 255 255 0 255 0 51 255 51 51 255 102 51 255 153 51 255 204 51 255 255 51 255 0 102 255 51 102 255 102 102 255 153 102 255 204 102 255 255 102 255 0 153 255 51 153 255 102 153 255 153 153 255 204 153 255 255 153 255 0 204 255 51 204 255 102 204 255 153 204 255 204 204 255 255 204 255 0 255 255 51 255 255 102 255 255 153 255 255 204 255 255 255 255 33 255 11 78 69 84 83 67 65 80 69 50 46 48 3 1 20 0 0 33 249 4 0 1 0 0 0 44 0 0 0 0 42 0 42 0 0 8 84 0 1 8 28 72 176 160 193 131 8 19 42 92 200 176 161 195 135 16 35 74 156 72 177 162 197 139 24 51 106 220 200 177 163 199 143 32 67 138 28 73 178 164 201 147 38 9 168 76 73 0 192 74 146 45 5 198 28 185 242 37 76 155 40 115 234 220 201 179 167 207 159 64 131 10 29 74 180 168 209 163 12 3 2 0 33 249 4 0 1 0 0 0 44 0 0 0 0 42 0 42 0 0 8 98 0 1 8 28 72 176 160 193 131 8 19 42 92 200 176 161 195 135 16 35 74 156 72 177 162 197 139 24 51 106 220 200 177 163 199 143 32 67 138 28 73 178 164 71 2 40 81 146 84 41 144 37 72 151 45 9 132 148 105 144 230 71 155 4 113 118 212 217 114 166 78 152 39 109 2 189 153 146 167 201 163 72 147 42 93 202 180 169 211 167 80 163 74 157 74 53 97 64 0 33 249 4 0 1 0 0 0 44 0 0 0 0 42 0 42 0 0 8 110 0 1 8 28 72 176 160 193 131 8 19 42 92 200 176 161 195 135 16 35 74 156 72 177 162 197 139 24 51 106 220 200 177 163 199 143 32 67 138 28 57 145 128 73 147 35 79 14 84 249 17 165 65 151 29 9 36 148 25 147 230 65 152 26 109 34 212 153 145 231 203 152 11 125 94 196 89 144 104 207 153 45 133 26 205 9 147 101 200 147 75 73 74 157 74 181 170 213 171 88 179 106 221 202 181 171 87 130 1 1 0 33 249 4 0 1 0 0 0 44 0 0 0 0 42 0 42 0 0 8 124 0 1 8 28 72 176 160 193 131 8 19 42 92 200 176 161 195 135 16 35 74 156 72 177 162 197 139 24 51 106 220 200 177 163 199 143 32 67 14 36 64 178 36 129 144 37 11 166 236 72 50 97 203 141 47 93 158 212 56 115 97 205 139 49 21 230 172 120 211 102 198 158 58 127 62 4 58 145 40 66 163 17 145 26 84 58 148 169 192 157 60 27 58 109 106 115 42 213 163 86 33 174 28 9 21 166 201 174 34 195 138 29 75 182 172 217 179 104 211 170 93 107 54 32 0 33 249 4 0 1 0 0 0 44 0 0 0 0 42 0 42 0 0 8 132 0 1 8 28 72 176 160 193 131 8 19 42 92 200 176 161 195 135 16 35 74 156 72 177 162 197 139 24 51 106 220 200 177 163 199 135 4 66 138 36 240 81 160 200 130 39 57 166 60 184 242 98 200 134 47 49 146 116 56 211 98 76 152 53 41 230 132 105 115 39 195 155 18 125 242 212 57 81 40 205 162 21 141 46 84 250 19 169 78 166 44 161 46 141 40 117 105 85 147 87 21 102 205 170 181 42 208 158 74 91 102 20 43 86 229 200 175 37 211 170 93 203 182 173 219 183 112 227 202 205 24 16 0 33 249 4 0 1 0 0 0 44 0 0 0 0 42 0 42 0 0 8 138 0 1 8 28 72 176 160 193 131 8 19 42 92 200 176 161 195 135 16 35 74 156 72 177 162 197 139 24 51 106 220 40 145 128 199 143 4 56 18 252 120 144 228 70 143 11 81 102 84 153 50 228 69 150 12 97 118 116 249 80 102 77 154 16 109 54 196 25 145 167 67 159 53 59 190 236 57 52 103 209 160 22 129 42 84 138 180 41 69 166 8 161 238 20 250 116 162 212 165 86 175 150 212 186 181 39 215 168 95 117 122 221 249 181 165 217 147 58 77 138 4 0 82 237 218 183 112 227 202 157 75 183 174 221 187 5 3 2 0 33 249 4 0 1 0 0 0 44 0 0 0 0 42 0 42 0 0 8 150 0 1 8 28 72 176 160 193 131 8 19 42 92 200 176 161 195 135 16 35 74 156 72 177 162 197 139 24 23 18 216 200 145 99 70 131 30 65 110 252 8 32 100 66 147 21 71 58 84 73 145 229 74 2 45 97 74 116 249 50 165 204 135 55 91 66 164 57 51 167 198 140 62 79 6 157 200 243 224 80 157 12 143 18 109 168 116 38 83 146 77 7 70 221 249 244 227 212 146 80 153 94 77 186 117 235 207 154 22 139 158 188 232 85 160 88 173 61 137 150 37 120 22 97 91 174 56 215 26 125 139 146 108 209 186 86 59 118 36 201 183 175 223 191 128 3 11 198 24 16 0 33 249 4 0 1 0 0 0 44 0 0 0 0 42 0 42 0 0 8 160 0 1 8 28 72 176 160 193 131 8 19 42 92 200 176 161 195 135 16 35 74 156 72 49 34 129 139 24 49 86 76 152 241 96 199 141 3 47 54 20 185 145 228 67 147 22 9 76 68 233 144 37 68 151 10 97 190 84 217 18 164 64 154 12 113 218 212 137 80 230 74 158 6 129 130 20 74 144 104 69 163 0 124 82 244 137 244 40 71 155 30 159 66 45 104 180 233 82 169 83 67 98 205 154 116 107 214 170 86 83 198 228 218 117 44 215 166 74 197 46 12 91 115 36 84 182 73 225 246 148 155 54 166 220 155 119 241 46 189 91 119 109 216 190 35 53 6 21 124 54 227 71 178 136 19 43 94 156 53 32 0 33 249 4 0 1 0 0 0 44 0 0 0 0 42 0 42 0 0 8 174 0 1 8 28 72 176 160 193 131 8 19 42 92 200 176 161 195 135 16 15 18 152 72 145 98 196 134 21 37 90 188 104 112 227 66 143 23 65 98 156 24 146 0 71 129 36 31 166 60 9 96 229 71 147 44 7 186 68 56 51 102 205 130 48 99 226 84 120 211 102 206 142 58 105 210 252 25 84 38 81 148 69 133 2 77 186 148 96 207 162 51 143 50 69 234 116 170 196 157 86 177 202 204 170 149 42 87 175 96 185 18 149 58 117 44 89 168 71 207 6 149 170 86 39 219 182 39 123 194 229 120 246 105 92 181 115 33 182 181 27 145 111 203 188 47 29 250 13 172 18 112 71 195 40 7 59 85 172 113 174 200 187 55 51 90 173 72 249 171 229 203 0 2 2 0 33 249 4 0 1 0 0 0 44 0 0 0 0 42 0 42 0 0 8 177 0 1 8 28 72 176 160 193 131 8 19 42 36 72 160 161 67 135 11 35 30 124 56 17 162 196 133 13 47 2 200 168 177 32 199 142 27 9 128 180 8 82 32 73 140 34 75 26 252 152 144 165 74 134 41 17 186 124 9 179 37 77 133 49 61 230 188 169 115 34 79 156 43 127 98 244 40 20 232 192 157 69 131 154 76 106 116 35 83 155 33 159 202 20 137 84 234 81 167 86 125 86 205 74 53 171 86 175 65 183 90 237 10 150 97 212 178 103 197 50 141 169 54 41 91 180 88 151 130 221 217 246 39 210 186 52 171 206 20 186 55 238 218 150 120 59 246 53 25 88 226 224 163 133 113 6 62 57 242 112 69 149 142 21 107 140 108 152 49 97 202 141 31 90 86 24 16 0 59].
]

{ #category : #accessing }
GIFReadWriterTest >> animatedColorFrames [
	"Responds with a collection of AnimatedImageFrames
	each containing equal-sized rectangles of green, red, 
	yellow. and black. When animated, these rectangles move
	into each other's positions, then back"
	| frames topLeft topRight bottomLeft bottomRight revFrames |
	frames := OrderedCollection new.
	topLeft := (0@0 extent: 200@200).
	topRight := (200@0 extent: 400@200).
	bottomLeft := (0@200 extent: 200@400).
	bottomRight := (200@200 extent: 400@400).
	
	0 to: 200 by: 20 do: [ :num |
		| newTopLeft newTopRight newBottomLeft newBottomRight canvas |
		newTopLeft := topLeft translateBy: (num@0).
		newTopRight := topRight translateBy: (0@num).
		newBottomLeft := (bottomLeft origin - (0@num)) corner: (bottomLeft extent - (0@num)).
		newBottomRight := (bottomRight origin - (num@0)) extent: (bottomLeft extent - (0@0)).
		canvas := FormCanvas extent: 400@400.
		canvas
			fillRectangle: newTopLeft color: Color green;
			fillRectangle: newTopRight color: Color red;
			fillRectangle: newBottomLeft color: Color yellow;
			fillRectangle: newBottomRight color: Color black.
		frames add: (AnimatedImageFrame new
			form: canvas form;
			disposal: #restoreBackground;
			delay: 60) ].

	revFrames := frames reversed.
	revFrames do: [ :f | frames add: f ].
	^ frames
]

{ #category : #tests }
GIFReadWriterTest >> testAnimatedColorsOutIn [
	"Ensure that the colored rectangles in the created
	animated gif are correct at different frames"
	| frames outBytes writer reader inBytes activeFrame |
	frames := self animatedColorFrames.
	
	"Write out the GIF bytes to the byte stream"
	outBytes := WriteStream on: (ByteArray new).
	writer := GIFReadWriter on: outBytes.
	frames do: [ :f | writer nextPutFrame: f ].
	writer close.
	
	"Read the GIF byte stream back into a GIF"
	inBytes := ReadStream on: (outBytes contents).
	reader := GIFReadWriter on: inBytes.
	reader
		readHeader;
		readBody.
	
	self assert: reader isAnimated equals: true.
	activeFrame := reader frames at: 1.
	self assert: (activeFrame form colorAt: 100@100) equals: Color green.
	self assert: (activeFrame form colorAt: 300@100) equals: Color red.
	self assert: (activeFrame form colorAt: 100@300) equals: Color yellow.
	self assert: (activeFrame form colorAt: 300@300) equals: Color black.
	activeFrame := reader frames at: 11.
	self assert: (activeFrame form colorAt: 100@100) equals: Color yellow.
	self assert: (activeFrame form colorAt: 300@100) equals: Color green.
	self assert: (activeFrame form colorAt: 100@300) equals: Color black.
	self assert: (activeFrame form colorAt: 300@300) equals: Color red.
	
]

{ #category : #tests }
GIFReadWriterTest >> testColorsFileOutIn [
	"Ensure that the colors that are written match up
	to the colors that are read in again"
	| canvas form outBytes inBytes writer reader |
	canvas := FormCanvas extent: 400@400.
	canvas
		fillRectangle: (0@0 extent: 200@200) color: Color green;
		fillRectangle: (200@0 extent: 400@200) color: Color red;
		fillRectangle: (0@200 extent: 200@400) color: Color yellow;
		fillRectangle: (200@200 extent: 400@400) color: Color black.
	form := canvas form.
	outBytes := 'testColorsFileOutIn.gif' asFileReference binaryWriteStream.
	writer := GIFReadWriter on: outBytes.
	writer
		nextPutImage: form;
		close.
	inBytes := 'testColorsFileOutIn.gif' asFileReference binaryReadStream.
	reader := GIFReadWriter on: inBytes.
	reader
		readHeader;
		readBody.
	inBytes close.
	'testColorsFileOutIn.gif' asFileReference delete.
	self assert: reader isAnimated equals: false.
	self assert: (reader form colorAt: 50@50) equals: Color green.
	self assert: (reader form colorAt: 250@50) equals: Color red.
	self assert: (reader form colorAt: 50@250) equals: Color yellow.
	self assert: (reader form colorAt: 250@250) equals: Color black.
]

{ #category : #tests }
GIFReadWriterTest >> testColorsOutIn [
	"Ensure that the colors that are written match up
	to the colors that are read in again"
	| canvas form outBytes inBytes writer reader |
	canvas := FormCanvas extent: 400@400.
	canvas
		fillRectangle: (0@0 extent: 200@200) color: Color green;
		fillRectangle: (200@0 extent: 400@200) color: Color red;
		fillRectangle: (0@200 extent: 200@400) color: Color yellow;
		fillRectangle: (200@200 extent: 400@400) color: Color black.
	form := canvas form.
	outBytes := WriteStream on: (ByteArray new).
	writer := GIFReadWriter on: outBytes.
	writer
		nextPutImage: form;
		close.
	outBytes := outBytes contents.
	inBytes := ReadStream on: outBytes.
	reader := GIFReadWriter on: inBytes.
	reader
		readHeader;
		readBody.
	self assert: reader isAnimated equals: false.
	self assert: (reader form colorAt: 50@50) equals: Color green.
	self assert: (reader form colorAt: 250@50) equals: Color red.
	self assert: (reader form colorAt: 50@250) equals: Color yellow.
	self assert: (reader form colorAt: 250@250) equals: Color black.
]
