# puzzle_game


This is a fillin puzzles Prolog program. A fillin puzzle will be represented as a list of lists, each of the same length and each representing a single
row of the puzzle. Each element in each of these lists is either a: '#', denoting a solid, unfillable square; an underscore (_), representing 
a fillable square; or a single, lower case letter (e.g., h), denoting a pre-filled square. A word list will be represented as a list of 
lists. Each list is a list of characters, spelling a word. The program supplies the predicate puzzle_solution(Puzzle, WordList) which 
holds when Puzzle is the representation of a solved fillin puzzle for the given, WordList, list of words.
The overall used method in this program is to prepare the puzzle by binding its slots, slots have '_' and not empty or solid'#',
with logical variables. Then find best fillable slots to fill the words in, and select the correct and the best word for each slot.
