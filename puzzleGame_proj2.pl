% Module: proj2
% Author: Gayda Mutahar, 1048309, gmutahar@student.unimelb.edu.au
% Last update: May, 31 2020
% Purpose: Prolog Program to slove a Fill-in puzzle
% COMP90048 Declarative Programming, Semester 1 2020, Project 2
% The University of Melbourne, School of Computing and Information Systems

% This is a fillin puzzles Prolog program. A fillin puzzle will be represented 
% as a list of lists, each of the same length and each representing a single
% row of the puzzle. Each element in each of these lists is either a: '#', 
% denoting a solid, unfillable square; an underscore (_), representing 
% a fillable square; or a single, lower case letter (e.g., h), 
% denoting a pre-filled square. A word list will be represented as a list of 
% lists. Each list is a list of characters, spelling a word.
%
% The program supplies the predicate puzzle_solution(Puzzle, WordList) which 
% holds when Puzzle is the representation of a solved fillin puzzle for
% the given, WordList, list of words.
%
% The overall used method in this program is to prepare the puzzle by binding
% its slots, slots have '_' and not empty or solid'#',
% with logical variables. Then find best fillable slots to fill the words in,
% and select the correct and the best word for each slot.

% ------------------------ The start of the program ------------------------ %

% load transpose/2 predicate to transpose a list of lists of the same length
:- ensure_loaded(library(clpfd)).


% -------------------------------------------------------------------------- %
% Solving the puzzle predicate 
% -------------------------------------------------------------------------- %

% puzzle_solution/2 perdicate sloves the puzzle by calling two predicates
% the first predicate creates the slots from the given puzzle 
% the second predicate fills in each slot with its corresponding word
puzzle_solution(Puzzle, WordList) :-
    create_slots_to_fill(Puzzle, Slots),
    fillin_slots(Slots, WordList).

% create_slots_to_fill/2 is a perdicate to prepare the puzzle's slots and 
% put them in one list we first get the horizontal slots from each row by put 
% the existing logical variables before using the function (transpose/2) 
% to transpose the given puzzle into a vertical puzzle. 
% Then vertical slots are created by the horizontal slots with its
% logical variables to get the ready fillable list of slots.
create_slots_to_fill(Puzzle, Slots) :-
    slots_dimension(Puzzle, [], HorizontalSlots),
    transpose(Puzzle, VerticalPuzzle),
    slots_dimension(VerticalPuzzle, HorizontalSlots, VerticalSlots),
    append(HorizontalSlots, VerticalSlots, Slots).

% -------------------------------------------------------------------------- %
% Slots builder predicates 
% -------------------------------------------------------------------------- %

% slots_dimension/3 predicate loops by each row of the given puzzle with its 
% logical variables using slots/4 predicate to create the list of slot 
slots_dimension([], Slots, Slots).
slots_dimension([Row|Rows], Slots, DimensionedSlots):-
    slots(Row, [], Slots, UpdatedSlots),
    slots_dimension(Rows, UpdatedSlots, DimensionedSlots).

% ---------------------------------------------- %

% slots/4 predicate is responsible for getting the fillable slots
% it has 5 clauses for different situation to meet different conditions
% 2 clauses to meet the situation of having empty slot list  
% 3 clauses to meet situation of having non-empty slot list 
% In the case of having non-empty slot list 4 cases are addressed as following:
% 1- Slot is '#', Word is empty list
% 2- Slot is '#', Word is non-empty, and lenght at most 1(only = 1 is possible)
% 3- Slot is '#', Word has lenght greater than 1
% 4- Slot is other than '#'

% clause 1: first base case It checks if word is longer than 1, then cuts 
% immediately and appending the word to the output slots list.
slots([], Word, Slots, CurrentSlots):-
    longer_than(Word, 1), !,
    append(Slots,[Word], CurrentSlots).

% clause 2: second base case that unifies input slots with the output slots. 
slots([], _, Slots, Slots).


% clause 3: Slot is '#', Word longer than 1.
slots([Slot|SlotsList], Word, Slots, FillableSlots):-
	Slot == '#', longer_than(Word, 1), !,
	append(Slots, [Word], UpdateSlots),
	slots(SlotsList,[],UpdateSlots, FillableSlots).

% First two cases of non-empty slot list can be merged in one clause 
% clause 4: Slot is '#', Word empty or single-character.
slots([Slot|SlotsList], _, Slots, FillableSlots):-
    Slot == '#', !,
    slots(SlotsList,[],Slots, FillableSlots).

% clause 5: Slot is different than '#'.
slots([Slot|SlotsList], Word, Slots, FillableSlots):-
    append(Word,[Slot], UpdateWord),
    slots(SlotsList, UpdateWord, Slots, FillableSlots).

% longer_than/2 is a helper predicate to take the list and number, and it holds
% only when length of the list is greater than given number
longer_than(L, N) :-
  length(L, K), K > N.

% -------------------------------------------------------------------------- %
% Fillin Slots predicates 
% -------------------------------------------------------------------------- %

% fillin_slots/2 predicate to start filling the slot by selecting the
% best slot that has to fill in words first based on its least possibility
% Only possible words would be used if they have not been still filled 
% and if they match a slot. At the same time, 2 filters will be applied as well
% one to delet the words that have been already filled from the word list
% the other filter to delete the filled slot from the fillable slot list
% then the predicate loops to fill in all the remaining slot with corresponding
% left words.
% it has 2 clauses (base case and recusrive case).
fillin_slots([],[]).
fillin_slots(Slots, WordList):-
    best_slot_tofill(Slots, WordList, BestSlot),
    (\+ member(BestSlot, WordList)
     -> 
      fillin_slots(BestSlot,WordList)
    ; 
      member(Word, WordList),
      BestSlot = Word,
      to_filter(Word, WordList, WordsLeft),
      to_filter(BestSlot, Slots, NextBestSlots),
      fillin_slots(NextBestSlots, WordsLeft)
    ).

% to_filter/3 helper predicate that filters the givenlist by deleting
% an element of it to get a new list without that element.
to_filter(_,[],[]).
to_filter(Elem, List, LeftList):-
    exclude(==(Elem), List, LeftList).

% ---------------------------------------------- %

% compute_matching_words/4 predicate to compute how many words match a slot 
% using its logical variables.
% it has 3 clauses 

% clasue 1: base case to handle the case of having no more words, so the
% current count is the final count
compute_matching_words(_,[],Count, Count).

% clasue 2: recursive clause to handle the case :if there is the word does not
% matching the slot, cut immediately and then loop to look for another word for 
% matching.
compute_matching_words(Slot,[Word|WordsList], Count, MatchWords):-
    \+ Slot = Word, !,
    compute_matching_words(Slot, WordsList, Count, MatchWords).

% clasue 3: recursive clause to handle the case : if there are matching words 
% for the slot, increasing the count by 1
% Then it loops to find all the other matching words and count them
compute_matching_words(Slot,[_|WordsList], Count, MatchWords):-
	UpdatedCount is Count + 1,
	compute_matching_words(Slot, WordsList, UpdatedCount, MatchWords).
% ---------------------------------------------- %

% find_best_slot/5 predicate to get the best slot by matching each fillable 
% slot with all possible words then choose the slot that has the least number 
% of maching words to be filled first.
% it has 3 clauses.

% clause 1: base case clause to handle the case of empty fillable slot list,
% so the current BestSlot is the slot to start with.
find_best_slot([],_,_, BestSlot, BestSlot).

% clause 2: recursive clause that updates current slot and its lenght, 
% when it found shorter than current otherwise clause 3 holds.
find_best_slot([Slot|SlotsList], WordList, SlotLen, _, BestSlot):-
	compute_matching_words(Slot, WordList, 0, MatchWords),
	SlotLen > MatchWords, !,
	find_best_slot(SlotsList, WordList, MatchWords, Slot, BestSlot).

% clause 3: recursive clause to skip slot from slots lists.
find_best_slot([_|SlotsList], WordList, SlotLen, CurrentSlot, BestSlot):-
	find_best_slot(SlotsList, WordList, SlotLen, CurrentSlot, BestSlot).

% ---------------------------------------------- %

% best_slot_tofill/3 predicate to start filling the slots. It holds by calling 
% compute_matching_words/4 and find_best_slot/5 in order to find the best slot
% to be filled first based on number of matching words  
best_slot_tofill([Slot|SlotsList], WordList, BestSlot):-
    compute_matching_words(Slot, WordList, 0, MatchWords),
    find_best_slot(SlotsList, WordList, MatchWords, Slot, BestSlot).
% ------------------------ The end of the program -------------------------- %