A DateModel is a widget for choosing dates. It consists of:
- A text box, into which you can type any string which can be converted into a Smalltalk date
- A button, which displays a calendar, from which you can select a date

Usage:
- the simplest way is to add a DateModel in your UI, and send #date when you want its value.
- or, register to be notified when the date changes:
	DateModel new
		date: self date;
		whenDateChanged: [ :newDate | self date: newDate ].

As it is Spec-based, it can be easily adapted and composed into a larger UI.