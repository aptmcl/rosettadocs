#lang scribble/manual
@(require "../rosetta.rkt")
@setup-math

@title{Structures}

@section{Introduction}

Most functions defined in the previous sections aim at producing specific
geometric shapes. In this section we will be addressing functions
which aim at creating abstract geometric shapes, in the sense of being
geometric shapes represented only by a cluster of positions in
space. For example, a polygon line can be represented only by the
sequence of positions through which it passes. That sequence of
positions can, however, be used for various other purposes such as to
create a sequence of spheres with centres in those positions or to
define the trajectory of a tube passing through those positions. These
examples show that the manipulation of these clusters of positions
is independent from the subsequent purpose we wish to give them.

In order to manage clusters of positions as a whole it is necessary to
group these positions in what is called @emph{data structures}. These
structures are particular arrangements of data that allow them to be
treated as a whole. A phone book for example, can be perceived as a
data structure which sets associations between names and phone numbers.

One of the most flexible data structures that we will be using exists
in many programming languages and is called @emph{list}.

@section{Lists}

A list is a sequence of elements. In Racket, we can build this
sequence of elements using the @fn[list] function. We can access
each element of the list by using the @fn[list-ref] function which,
given a list and a position of an item of that list, tells us which
element that is. Here is an example of the use of these operations:

@incremental[
(define friends (list "Michael" "Peter" "Charles" "Mary")) 
(list-ref friends 0)
(list-ref friends 3)
]

As we can see by the previous example, the @fn[list-ref] function
considers that the first element of a list occupies the zero position.
The @fn[list] function accepts any number of arguments,
grouping them all in the same list. Besides these two, there are
many other functions for manipulating lists. Let us
first introduce the @fn[cons] function, whose name is an
abbreviation of the word @emph{construct}, that allows us to create a
larger list from an element and a list to which we wish to add it:

@incremental[
(define new-friends (cons "John" friends))
new-friends
friends
]

As we can see, the @fn[cons] function adds an element to a list,
producing a new list that incorporates the new element at the head of
the list. We also note that the original list remains unchanged by the
function @fn[cons]. It is also important to note that when Racket returns
a list as a result, it starts by writing an apostrophe (@verb{'}) and then
writes the elements of the list between parentheses, as if it was writing a
combination. The apostrophe's purpose is to indicate that the result is
not a combination but a list. @footnote{The fact that Racket uses the
same notation for both combinations and lists is not accidental, and due
to us being able to have programs that create other programs.} This
clarification is crucial when we use this notation because, without it,
Racket would think that we are writing a combination to be evaluated,
following the regular rules of evaluating combinations. Thus, the
apostrophe informs Racket that we do not wish the usual evaluation
procedure but rather to construct a list, as seen in following interaction:

@incremental[
(+ 1 2)
'(+ 1 2)
]

In order to "decompose" lists, Racket provides a pair of functions
--- @fn[car] and @fn[cdr]--- which can be seen as the
inverse of the @fn[cons] function. Whilst the @fn[cons] function
joins an element to a list, the @fn[car] function indicates which
element was joined and the @fn[cdr] function indicates to which list it
was joined. @footnote{Some Lisp dialects, including Racket, provide the
synonyms @fn[first] and @fn[rest] for the @fn[car] and
@fn[cdr] functions respectively.} Here is an example:

@incremental[
(car new-friends)
(cdr new-friends)
]

In other words, it is possible to say that the @fn[car] function
returns the @emph{first} element of the list and the @fn[cdr]
function returns the @emph{rest} of the list, i.e., the list after the
first element.

Naturally, the functions @fn[car] and @fn[cdr] can be chained:

@incremental[
(car (cdr new-friends))
(cdr (cdr (cdr new-friends)))
(car (cdr (cdr (cdr new-friends))))
]

Given that these kind of expressions are widely used in Racket, many
combinations were created. The @fn[caddr] function, for example, is
defined so that @lisp[(caddr #,(lispemph exp))] corresponds to
@lisp[(car (cdr (cdr #,(lispemph exp))))]. The name of the function
indicates which operations are to be performed. An "a" represents a
@fn[car] and a "d" represents a @fn[cdr].

From the previous examples, we can deduce that the @fn[list]
function does something very similar to a combination of the
@fn[cons] function:

@incremental[
(cons 1 (list 2 3 4))
(cons 1 (cons 2 (list 3 4)))
(cons 1 (cons 2 (cons 3 (list 4))))
(cons 1 (cons 2 (cons 3 (cons 4 (list)))))
]

There is but a single case when the @fn[list] function cannot be
decomposed into a composition of @fn[cons] functions: the expression
@verb{(list)} produces a list with no elements, i.e., an @emph{empty
list}. Logicaly, Racket considers that the form @lisp['()] represents a
list with no elements. In fact, when we try to obtain the rest of a list
with a single element we get @lisp['()], as when we try to build a list
without elements:

@incremental[
(cdr (list 1))
(list)
]

As we will see, the functions that we will define will have to deal
with empty lists, so we have to be able to recognize them.  For this
purpose we have the predicate @fn[null?] which is true when
applied to an empty list and false otherwise. @footnote{Some Lisp
dialects, Racket included, provide a synonym for this function called
@fn[empty?].}

@incremental[
(null? (list))
(null? friends)
]

@subsection{Pairs}

We have seen that the @fn[cons] function allows an element to be
added to a list but actually it does something far more
fundamental. This function accepts any two entities as arguments and
produces a par with these two entities, i.e., a value representing a
cluster of those two entities. It is common to say that this pair is
a @emph{cons}. The @fn[car] and @fn[cdr] functions are merely the
@emph{selectors} that return the first and second element of that
pair. Here it is an example of the creation of a pair of numbers:

@incremental[
(cons 1 2)
(car (cons 1 2))
(cdr (cons 1 2))
]

Note that when Racket pretends to write the result of a pair creation,
it begins by writing the apostrophe followed by an open parentheses,
then, the first element of the pair, a point to separate, the second
element of the pair, and finally a closing parenthesis. This notation
is called "pair with point" or, as the original, @emph{dotted
pair}. However, when the second element of the pair is also a pair,
Racket employs a simpler notation that omits the point and a pair of
parentheses:

@incremental[
(cons 1 (cons 2 3))
]

In the previous example, if Racket was to write it using the pair with dot
notation, it would have to write @verb{'(1. (2. 3))}.

Finally, when the second element of the pair is an empty list, Racket only
writes the the first element between a pair of parenthesis.

@incremental[
(cons 1 '())
]

It is the combination of these two rules that allow us to visualize a
sequence of pairs ending with an empty list as being a list:

@incremental[
(cons 1 (cons 2 (cons 3 '())))
]

@subsection{Graphic Representation of Pairs}

As stated above, when we write @lisp[(cons #,(math-in "\\alpha") #,(math-in "\\beta"))], we
are creating a pair with the @${\alpha} and @${\beta} elements. This implies
that memory will be reserved in our program to contain this pair, that we
graphically represent with a box divided into two halves, the one on the left
pointing to the first @fn[cons] argument (the @emph{car}) and the one on the
right pointing to the second @fn[cons] argument (the @emph{cdr}). For example,
the expression @lisp[(cons 1 "two")] can be represented in this graphical
notation, called @emph{box and link}, as follows:

@fig[@tex{
\begin{tikzpicture}
\conscell{(0,0)}{1}
\car{(-1,-1)}{1}{\lisp{1}}
\cdr{(1,-1)}{1}{\lisp{"two"}}
\end{tikzpicture}
}]

Obviously, a @emph{cons} can point to another @emph{cons}. That is
precisely what happens when we create lists. As we have seen, a list is
no more than a @emph{cons} in which the @emph{cdr} is directly linked to
another list (empty or not). For example, the list created by the
expression @lisp[(list 1 2 3)] is equivalent to the list @lisp[(cons 1
(cons 2 (cons 3 '())))]. Its graphical representation is:

@fig[@tex{
\begin{tikzpicture}
\conscell{(0,0)}{1}
\car{(-0.25,-1)}{1}{\texttt{1}}
\conscell{(2,0)}{2}
\consarrow{1 cdr.center}{2 car}
\car{(1.75,-1)}{2}{\texttt{2}}
\conscell{(4,0)}{3}
\consarrow{2 cdr.center}{3 car}
\car{(3.75,-1)}{3}{\texttt{3}}
\cdr{(6,0)}{3}{\texttt{'()}}
\end{tikzpicture}
}]

Here we see that from Racket's point of view a list is nothing more than
a particular arrangement of pairs wherein the second element of each pair
is, either another pair, or an empty list.

Thus, the empty list @lisp['()] is the starting point for building
any list, as we can see in the following sequence of expressions:

@incremental[
(cons 3 '())
(cons 2 (cons 3 '()))
(cons 1 (cons 2 (cons 3 '())))
]

Note that in the above expressions the second argument of the
@fn[cons] function is either an empty list or a list containing some
elements. In these cases, the result of invoking the @fn[cons]
function will always be a list.

@section{Recursive Types}

Obviously, whichever the list we come up with, it is always possible to
create it only by using the @fn[cons] function and the @lisp['()] empty
list. In fact, @lisp[(list)] is equivalent to @lisp['()] and @lisp[(list
#,(lispemphi e "1") #,(lispemphi e "2") ... #,(lispemphi e "n"))] is
equivalent to @lisp[(cons #,(lispemphi e "1") (list #,(lispemphi e "2")
... #,(lispemphi e "n")))].

Consequently, we can say that a list is always:

@itemlist[

@item{either an empty list @lisp['()],}

@item{or a @fn[cons] of one element to a list.}

]

Note that there is a subtle detail in the list definition that we have
just presented: it is recursive! To confirm this we just need to note
that we are defining a @emph{list} as a @fn[cons] of one element to
a @emph{list}, i.e. we use the term we want to define in its own
definition. As we know from any recursive definition, it is necessary to
have a stopping condition which, in the case of lists, is the empty list.

When a data type is defined recursively it is usually called
@emph{recursive type}. Lists are, therefore, a recursive type. In
a recursive type there must always be a primal element from which the
remaining elements are created. That primal element is called
@emph{primitive element}. In case of lists, the primitive element is,
obviously, the empty list.

As with other types of data, the various operations for manipulating
lists can be classified as constructors, selectors and recognizers. It is
easy to see that @lisp['()] and @fn[cons] are constructors, @lisp[car] and
@fn[cdr] are selectors, and finally @fn[null?] is a recognizer. Using
these operations it is possible to define several other operations that
operate on lists.

@section{Recursion in Lists}

One of the interesting properties of recursive types is that all
operations that process elements of recursive type tend to be
implemented by recursive functions. For example, if we wish to define a
function that tells us how many elements there are in a list, we have
to think recursively:

@itemlist[

@item{If the list is empty, then the number of elements is obviously zero.}

@item{Otherwise, the number of elements is one plus the number of
remaining element of the @emph{rest} of the list} ]

To assure the correctness of our reasoning we must ensure that the
assumptions applied to every recursive definitions can be verified,
namely:

@itemlist[

@item{There is a reduction of the problem's complexity in every recursive
invocation: in fact, in each recursive invocation the list becomes
smaller.}

@item{There is the most simple case of all where answer is immediate:
when the list is empty, the answer is zero.}

@item{There is an equivalence between the original problem and the use
of the recursion's result: it is true that the number of elements in a
list is the same as adding @${1} to the number of elements in that
list without the first element.}
]

The verification of these assumptions is sufficient to make sure
the function is correct.

Translated into Racket we have:

@def[
(define (number-of-elements list)
  (if (null? list)
    0
    (+ 1 (number-of-elements (cdr list)))))
]

An example:

@incremental[
(number-of-elements (list 1 2 3 4))
(number-of-elements (list))
]

It is important to note that the existence of sublists does not change
the number of elements in a list. In fact, the elements of sublists are
@emph{not} considered elements of the list. For example:

@incremental[
(list (list 1 2) (list 3 4 5 6))
(number-of-elements (list (list 1 2) (list 3 4 5 6)))
]

In the previous example we can see that the list only contains two
elements, despite each of them being lists with more elements. In fact,
the @fn[number-of-elements] function already exists in Racket with the name
@fn[length].

Another useful operation is the one that allows us to obtain the
@${n}th element of a list. We saw that this function already exists
and is called @fn[list-ref] but let us define our own version. It is
common to assume that for @${n=0}, one should obtain the first element
of the list. To define this function we should once again think
recursively:

@itemlist[

@item{If @${n} is zero we return the first element of the list.}

@item{Otherwise, to return the @${n}th element of the list is the
same as to return the @${(n-1)}th element of the @emph{rest} of the
list.}
]

Once again we notice that there is a decrease of the problem, that the
reduction is equivalent to the original problem and the problem will
approach the basic case. In Racket, we have:

@def[
(define (nth n list)
  (if (= n 0)
    (car list)
    (nth (- n 1) (cdr list))))
]

@incremental[
(nth 2 '(first second third fourth fifth))
]

Once again, the previous function already exists in Racket with the name
@fn[list-ref].

It is often necessary to concatenate lists. To do so, we can
imagine a @fn[concatenate] function that, given two lists, returns
a list containing the elements of the two lists, in the same
order. For example:

@def/no-show[
(define (concatenate l1 l2)
  (if (null? l1)
    l2
    (cons (car l1)
          (concatenate (cdr l1) l2))))
]

@incremental[
(concatenate (list 1 2 3) (list 4 5 6 7))
]

As always, recursions helps solve the problem.
Let us start by thinking how the problem can be simplified,
i.e., in turning the original problem @lisp[(concatenate (list 1
2 3) (list 4 5 6 7))] into a slightly simpler problem.

Since there are two lists as parameters, we can simplify the problem
by making a reduction in one of them, i.e., we can consider a
reduction on the first list:

@lispcode[
(concatenate (list 2 3) (list 4 5 6 7))
]

of which the result is @lisp[(2 3 4 5 6 7)]., or, alternatively, a
reduction on the second list:

@lispcode[
(concatenate (list 1 2 3) (list 5 6 7))
]

of which the results in @lisp['(1 2 3 5 6 7)].

Besides simplifying the problem, it is also necessary to ensure
that we can find a way to make the problem's simplification equivalent
to the original problem. For the first case that is trivial, one must
simply add the number @lisp[1] to the beginning of the list:

@lispcode[
(cons 1 (concatenate (list 2 3) (list 4 5 6 7)))
]

For the second alternative that is much more difficult to do since we
would have to insert the number @lisp[4] somewhere in the middle of the
resulting list.

Thus, there should be no doubts that the function should take the form of:

@lispcode[
(define (concatenate l1 l2)
  (if ???
    ???
    (cons (car l1)
          (concatenate (cdr l1) l2))))
]

We now need to define the basic case. In order to do so, we need only
think that if we are reducing the first list in each recursive call, then
there will come a moment when the first list is empty. What is then
the concatenation of an empty list with any other list? Obviously it
is the other list. So, we can completely define the function:

@def[
(define (concatenate l1 l2)
  (if (null? l1)
    l2
    (cons (car l1)
          (concatenate (cdr l1) l2))))
]


In fact, this function already exists in Racket with the name
@fn[append]. It has the additional advantage of receiving any number
of arguments, for example:

@incremental[
(append (list 0) (list 1 2 3) (list 4 5) (list 6 7 8 9))
]

The inverse operation of concatenating lists is separating them. To keep it
simple let us consider a function that obtains a sublist from a given list.
This sublist includes all elements between two given indexes. Usually,
the convention is that the sublist includes the element with the first index
and @emph{excludes} the second indexed element. For example:

@def/no-show[
(define (sublist l begining end)
  (cond ((> begining 0)
	 (sublist (cdr l) (- begining 1) (- end 1)))
	((> end 0)
	 (cons (car l)
	       (sublist (cdr l) begining (- end 1))))
	(else
	 (list))))
]

@incremental[
(sublist (list 0 1 2 3 4 5 6) 2 4)
]

To define this function, we can recursively simplify the problem of
obtaining the sublist @lisp[(#,(lispemphi e "0") #,(lispemphi e "1")
... #,(lispemphi e "n"))] between the indexes @${i} and @${j} into
the problem of obtaining the sublist @lisp[#,(lispemphi e "1") ...
#,(lispemphi e "n")] between the indexes @${i-1} and @${j-1}. When @${i}
is zero, then the problem is transformed into obtaining the sublist of
@lisp[(#,(lispemphi e "0") #,(lispemphi e "1") \ldots{} #,(lispemphi e "n"))]
between the indexes @${0} and @${j} to become the @fn[cons] of
@(lispemphi e "0") with the sublist of @lisp[(#,(lispemphi e "1") \ldots{}
#,(lispemphi e "n"))] between the indexes @${0} and @${j-1}. Translating
this algorithm to Racket, we have:

@def[
(define (sublist l begining end)
  (cond ((> begining 0)
	 (sublist (cdr l) (- begining 1) (- end 1)))
	((> end 0)
	 (cons (car l)
	       (sublist (cdr l) begining (- end 1))))
	(else
	 (list))))
]

@questions[
@question{Define a @fn[delete-nth] function that receives a number @${n}
and a list, and deletes the @${n}th element of the list. Note
that the the first element of the list corresponds to @${n=0}.

For example:

@def/no-show[
(define (delete-nth n list)
  (if (= n 0)
    (cdr list)
    (cons (car list) (delete-nth (- n 1) (cdr list)))))
]

@incremental[
(delete-nth 2 (list 0 1 2 3 4 5))
]}

@question{ Write a @fn[change-nth] function that receives a number @${n},
a list and an element and replaces the @${n}th number of the
list by the given element. Note that the first element of the list
corresponds to @${n=0}. 

For example:

@def/no-show[
(define (change-nth n list elem)
  (if (= n 0)
    (cons elem (cdr list))
    (cons (car list) (change-nth (- n 1) (cdr list) elem))))
]

@incremental[
(change-nth 2 (list 0 1 2 3 4 5) 9)
(change-nth 2 (list "I am" "going" "to Coimbra") "to Lisbon")
]
}

@question{ Write a function that given a list of elements returns an
element of that list chosen randomly

@def/no-show[
(define (random-element list)
  (list-ref list (random (length list))))
]
}

@question{ Define the @fn[one-of-each] function that, given a list
of lists, creates a list, in order, with a random element of each list.
For example:

@def/no-show[
(define (one-of-each ls)
  (if (null? ls)
    (list)
    (cons (random-element (car ls))
	  (one-of-each (cdr ls)))))
]

@incremental[
(one-of-each (list (list 0 1 2) (list 3 4) (list 5 6 7 8)))
(one-of-each (list (list 0 1 2) (list 3 4) (list 5 6 7 8)))
(one-of-each (list (list 0 1 2) (list 3 4) (list 5 6 7 8)))
]
}

@question{ Define the @fn[random-elements] function that, given a number
@${n} and a list of elements returns @${n} elements of that
list picked randomly. For example:

@def/no-show[
(define (random-elements n l)
  (if (= n 0)
    (list)
    (let ((i (random (length l))))
      (cons (nth i l)
            (random-elements
              (- n 1)
              (delete-nth i l))))))
]


@incremental[
(random-elements 3 (list 0 1 2 3 4 5 6))
]
}

@question{ Redefine the @fn[random-elements] function so that the
result respects the order of the position that the elements had in the
list from which they were chosen.  }

@question{ As we have seen, the @fn[cons] function adds a new first
element to a list, the @fn[car] returns the first element of a list
and the @fn[cdr] function returns the list without the first
element.

Write the "reverse" of the @fn[cons], @fn[car] and @fn[cdr]
functions, called @fn[snoc], @fn[rac] and @fn[rdc] that, instead
of operating with the first element, operate with the last one. The
@fn[snoc] function receives an element and a list and adds the
element to the end of the list. The @fn[rac] returns the last
element of the list. And the @fn[rdc] returns a list containing all
elements except the last one.  }

@question{ Define a @fn[reverse] function that receives a list and
returns another list which has the same elements as the given list
presented in reverse order.  }
]

@section{Predicates on Lists}

To test whether a particular entity is a list we can use the
predicate @fn[list?]. This predicate is true for any
list (including the empty list) and false for everything else:

@incremental[
(list? (list 1 2))
(list? 1)
(list? (list))
(list? (cons 1 2))
]

In the last example, notice that the universal recognizer @fn[list?]
does not consider a simple pair of elements a list. For that, there is
the predicate @fn[pair?] that tests if the argument is a
@emph{dotted pair}:

@incremental[
(pair? (cons 1 2))
(pair? (list 1 2))
(pair? (list))
]

@questions[
@question{ Given that lists are flexible data structures, very often
we use lists containing other lists, which in turn may contain other
lists, up to the depth level that we like. For example, consider the
following list of positions:


@incremental[
(list (list 1 2) (list 3 4) (list 5 6))
]

or the following list of lists of positions:

@incremental[
(list (list (list 1 2) (list 3 4) (list 5 6))
      (list (list 7 8) (list 9 0)))
]

Write a function called @fn[flatten] that receives a list (possibly
containing sublists) as argument and returns another list with all the
elements of the first list and in the same order, i.e.:

@def/no-show[
(define (flatten list)
  (cond ((null? list)
         (list))
        ((pair? list)
         (append (flatten (car list))
                 (flatten (cdr list))))
        (else 
         (list list))))
]

@incremental[
(flatten '(1 2 (3 4 (5 6)) 7))
]}

@question{ The concatenation of @emph{strings} can be performed by the
@fn[string-append] function. Define the @fn[concatenate-strings]
function that, given a list of @emph{strings}, returns a single
@emph{string} with the concatenation of all strings contained in the
list. For example:

@def/no-show[
(define (concatenate-strings strings)
  (if (null? strings)
    ""
    (string-append (car strings)
	           (concatenate-strings (cdr strings)))))
]

@incremental[(concatenate-strings (list "A" "view" "of" "the" "Sintra" "Mountains"))]}

@question{ The @fn[concatenate-strings] function "glues" strings to
each other without leaving any space between them. Define a new
function called @fn[create-sentence] that receives a list of
@emph{strings} and concatenates them leaving a space every other
word. For example:

@def/no-show[
(define (create-sentence words)
  (cond ((null? words)
	 "")
	((null? (cdr words))
	 (car words))
	(else
	 (string-append (car words)
                        " "
                        (create-sentence (cdr words))))))
]


@incremental[(create-sentence (list "The" "view" "of" "the" "Sintra" "hills"))]
}

@question{ Define the @fn[random-sentence] function that receives a
list of lists of words and returns a random sentence composed by words
from each list. For example, the repeated evaluation of the
following expression:

@def/no-show[
(define (random-sentence words)
  (create-sentence (one-of-each words)))
]

@lispcode[
(random-sentence
   (list (list "AutoLisp" "Scheme" "Racket")
         (list "is a" "was always a" "remains a")
         (list "fantastic" "fabulous" "modern")
         (list "language.")))
]

can produce the following results:

@lispcode[
"Racket is a modern language."
"Racket is a fantastic language."
"Scheme was always a modern language."
"AutoLisp remains a fantastic language."
"AutoLisp is a modern language."
"Scheme is a fantastic language."
"Scheme remains a great language."
]

Tip: Define the @fn[random-sentence] function using the
@fn[create-sentence] and @fn[one-of-each] functions.}

@question{ Consider creating a function for giving speeches. The basic
idea giving a speech is to say the same thing in different ways and
do not seem repetitive. Here is an example of the desired interaction:


@def/no-show[
(define (random-speech)
  (random-sentence
   (list (list "Dear" "Esteemed")
	 (list "friends." "colleagues." "fellows.")
	 (list "It is with")
	 (list "immense" "great" "huge")
	 (list "pleasure" "joy")
	 (list "that")
	 (list "I see" "I find myself reunited with")
	 (list "you again")
	 (list "in this place." "here." "in this room."))))
]


@incremental[(random-speech)(random-speech)(random-speech)
(random-speech)(random-speech)(random-speech)(random-speech)(random-speech)]

Study the presented interaction and define the @fn[random-speech]
function.}
]

@section[#:tag "sec:enumera"]{Enumerations}

Let us now consider a function that will be useful in the future for
enumerating. Given the limits @${a} and @${b} of an interval @${[a, b]}
and an increment @${i}, the function should return a list of all
numbers from @${a}, @${a+i}, @${a+2i}, @${a+3i}, @${a+4i}, @ldots, to
@${a+ni>b}.

Once again, recursion helps: the enumeration of the numbers in the interval
@${[a, b]} with an @${i} increment is exactly the same as the number
@${a} followed by the enumeration of the numbers in the interval
@${[a + i, b]}. The simplest case of all is an enumeration in the interval
@${[a, b]} wherein @${a>b}. In this case the result is simply the
empty list.

This function's definition is now trivial:

@def[
(define (enumerate a b i)
  (if (> a b)
    (list)
    (cons a
          (enumerate (+ a i) b i))))
]


As an example:

@incremental[(enumerate 1 5 1)
(enumerate 1 5 2)
]

To make the function even more generic we can also cover the case where
@${d} is a negative increment. This is useful, for example, for a regressive
counting: @lisp[(enumerate 10 0 -1)].

Note that the current definition of the @fn[enumerate] function
does not allow this because the use of a negative increment causes an
infinite recursion. The problem lies in the fact that the stopping test
is made with the function @fn[>] that is only suitable for the case in which
the increment is positive. In the case of a negative increment we
should use @fn[<]. So, to solve the problem we must first identify
which is the correct operation for the comparison, something we can do
with a simple test:

@def[
(define (enumerate a b i)
  (if ((if (> i 0)
         >
         <)
        a b)
    (list)
    (cons a
          (enumerate (+ a i) b i))))
]

Now we can have:

@incremental[
(enumerate 1 5 1)
(enumerate 5 1 -1)
(enumerate 6 0 -2)]

@questions[
@question{ Unfortunately, the @fn[enumerate] function is unnecessarily
inefficient because, despite the fact that the increment never changes
along the recursive calls, we are systematically testing if it is
positive. Obviously there is only the need to test it once in order to
decide, once and for all, what test we should perform. Redefine the
@fn[enumerate] function so that no unnecessary tests are performed.

@def/no-show[
(define (enumerate a b i)
  (define (enumerate-up a)
    (if (> a b)
      (list)
      (cons a
            (enumerate-up (+ a i)))))
  (define (enumerate-down a)
    (if (< a b)
      (list)
      (cons a
            (enumerate-down (+ a i)))))
  (if (> i 0)
    (enumerate-up a)
    (enumerate-down a)))
]
}

@question{ The @${iota} function (pronounced "iota") is an
enumeration starting from @${0} to an upper limit @${n},
@emph{excluding} the @${n} value, with the elements of the enumeration
separated by a given increment, i.e.:

@def/no-show[
(define (iota b i)
  (enumerate 0 (- b i) i))
]

@incremental[(iota 10 1)(iota 10 2)]

Define the @fn[iota] function using the @fn[enumerate] function.
}

@question{ The @fn[enumerate] function can have a bizarre behaviour
when the increment is not an exact number. For example:

@incremental[(length (enumerate 0 1 1/100))
(length (enumerate 0 1 0.01))]

As we can see, when we use @${\frac{1}{100}}, i.e., an @emph{exact}
hundredth, the function produces a list with the correct
length. However, when we use @${0.01}, an @emph{inexact} hundredth,
the function produces a list with one element missing. This is due to
the fact that the number @${0.01} cannot be represented exactly in
binary notation, and so, as a sufficiently large amount of these
numbers are added, a significant error is accumulated.

The Kahan algorithm allows us to minimize this problem. Do a search for this
algorithm and use it to implement a new version of the
@fn[enumerate] function capable of avoiding accumulating errors when
the increment is not an exact number.  }

@question{ Define a function called @fn[member?] that receives a
number and a list and checks whether that number exists in the given
list. Here are two examples of the use of said function:

@def/no-show[
(define (member? num l)
   (cond ((null? l) #f)
         ((= num (car l)) #t)
         (else (member? num (cdr l)))))
]

@incremental[(member? 3 '(5 2 1 3 4 6)) (member? 7 '(5 2 1 3 4 6))]}

@question{ Define a function called @fn[eliminate1] that receives
as arguments a number and a list and returns as a result another
list in which the first occurrence of that number was eliminated. Here
is an example:

@def/no-show[
(define (eliminate1 num l)
   (cond ((null? l) 
          (list))
         ((= num (car l))
          (cdr l))
         (else 
          (cons (car l)
                (eliminate1 num (cdr l))))))
]

@incremental[(eliminate1 3 '(1 2 3 4 3 2 1))]
}

@question{ Define a function called @fn[eliminate] that receives,
as arguments a number and a list and returns as a result another
list in which that number is completely eliminated. Here is an
example:

@def/no-show[
(define (eliminate num l)
   (cond ((null? l) 
          (list))
         ((= num (car l))
          (eliminate num (cdr l)))
         (else 
          (cons (car l)
                (eliminate num (cdr l))))))
]

@incremental[(eliminate 3 '(1 2 3 4 3 2 1))]}

@question{ Define a function called @fn[replace] that receives two
numbers and a list as arguments and returns another list with
all the occurrences of the second argument replaced with the ones in
the first one. Here is an example:

@def/no-show[
(define (replace new old l)
   (cond ((null? l) (list))
         ((= old (car l))
          (cons new (replace new old (cdr l))))
         (else
          (cons (car l)
                (replace new old (cdr l))))))
]

@incremental[(replace 0 1 '(1 2 1 3)) (replace 0 1 '(2 3 4 5))]}

@question{ Define a function called @fn[remove-duplicates] that
receives a list of numbers as argument and returns another list with all
the elements of the first one but without repeated numbers, i.e.:

@def/no-show[
(define (remove-duplicates l)
   (cond ((null? l) (list))
         ((member (car l) (cdr l))
          (remove-duplicates (cdr l)))
         (else
          (cons (car l)
                (remove-duplicates (cdr l))))))
]

@incremental[(remove-duplicates '(1 2 3 3 2 4 5 4 1))]}

@question{ Define a function called @fn[occurrences] that receives
a number and a list as arguments and returns the number of occurrences
of that number in the given list. Here is an example:

@def/no-show[
(define (occurrences num l)
   (cond ((null? l) 0)
         ((= num (car l))
          (+ 1 (occurrences num (cdr l))))
         (else
          (occurrences num (cdr l)))))
]

@incremental[(occurrences 4 '(1 2 3 3 2 4 5 4 1))]}

@question{ Define a function called @fn[position] that receives a
number and a list as arguments and returns the position of the first
occurrence of that number in the given list. Note that positions in a
list start at zero. For example:

@def/no-show[
(define (position num l)
   (cond ((= num (car l))
          0)
         (else
          (+ 1 (position num (cdr l))))))
]

@incremental[(position 4 '(1 2 3 3 2 4 5 4 1))]}
]

@section[#:tag "sec:polygons"]{Polygon}

Lists are particularly useful to represent abstract geometrical
entities, i.e., entities from which we are only interested in knowing
certain properties such as, for example, their position. To do so we
can use a list of positions, i.e., a list where the elements are the
result of calling constructors of coordinates.

Let us imagine as as example that we want to represent a
@emph{polygon}. By definition, a polygon is a plane figure bounded by
a closed path composed by a sequence of line segments. Each line segment
is an @emph{edge} (or @emph{side}) of the polygon. Each point where
two line segments meet is a @emph{vertex} of the polygon.

Based on the polygon's definition it is easy to infer that one of the
simplest ways to represent a polygon will be through a sequence of
positions that indicate in which order we should connect the vertices
with an edge and admitting that the last element will be connected to
and first one. It is precisely this sequence of arguments that we provide
in the following example, where we use the pre-defined @fn[polygon] function:

@def/no-results[
(polygon (xy -2 -1) (xy 0  2) (xy 2 -1))
(polygon (xy -2  1) (xy 0 -2) (xy 2  1))
]

@figure[#:tag "fig:poligono1"
        #:caption @elem{Two overlapping polygons.}]{
@(show-tikz 1)
}

Suppose that, rather than explicitly indicate the vertices positions,
we want a function to compute them. Naturally, that would require the
function to compute a sequence of positions. It is precisely here that
lists come to give an valuable help: they allow the representation of
sequences and, to further simplify their use, many of the
geometric functions, such as @fn[line] and @fn[polygon], also accept
lists as argument. In fact, @figref{fig:poligono1} can also be
obtained by the following expressions:

@def/no-results[
(polygon (list (xy -2 -1) (xy 0  2) (xy 2 -1)))
(polygon (list (xy -2  1) (xy 0 -2) (xy 2  1)))
]

@(clear-tikz)

Thus, a list with the positions of four vertices can represent a
quadrilateral, an octagon can be represented by a list of eight
vertices, a triacontakaihenagon can be represented by a list of
thirty-one vertices, etc.. Now, we need only focus on creating these
lists of positions.

@subsection{Regular Stars}

The polygon shown in @figref{fig:poligono1} was generated
"manually" by overlapping two triangles. A more interesting polygon is
the famous @emph{pentagram}, or five point star, that has been used
with symbolic, magical or decorative purposes since the Babylon
times. @footnote{The pentagram was a mathematical symbol for
perfection to the Pythagoreans, was a symbol of spiritual
mastery over the four natural elements to cultists, the symbol of
the Five Holy Wounds of Christ to the Christians, was associated with the
proportions of the human body, has been used by the Freemasonry and, when
inverted, was associated with Satanism.} @figref{fig:etcgram}
demonstrates the use of the pentagram (as well as the octagram, the
octagon and the hexagon) as a part of a structural and decorative
window made of stone.

@figure[#:tag "fig:etcgram"
        #:caption @elem{Variations of stars in the Amber Fort window located in the state of Jaipur, India. Photography by David Emmett Cooley}]{
  @authorizedPhoto{estrelas/DavidEmmettColey}
}

Apart from the extra-geometric connotations, the pentagram is, first
of all, a polygon. For this reason it is drawable by the @fn[polygon]
function, as long as we can produce a list with the positions of the
vertices. In order to do so, let us focus on @figref{fig:pentagrama0},
where it is possible to see that the five
pentagram vertices divide the circle into 5 parts, with arches
measuring @${\frac{2\pi}{5}} each. From the pentagram's centre, its
top vertex makes an angle of @${\frac{\pi}{2}} with the abscissas
axis.  This vertex must be connected, not with the next vertex, but
with the one immediately after it, i.e., after a rotation of two arcs
or @${\frac{2\cdot 2\pi}{5}=\frac{4}{5}\pi}. We can now define the
@fn[pentagram-vertices] function that creates the list of vertices of
the pentagram:

@def/no-results[
(define (pentagram-vertices p r)
  (list (+pol p r (+ (/ pi 2) (* 0 4/5 pi)))
        (+pol p r (+ (/ pi 2) (* 1 4/5 pi)))
        (+pol p r (+ (/ pi 2) (* 2 4/5 pi)))
        (+pol p r (+ (/ pi 2) (* 3 4/5 pi)))
        (+pol p r (+ (/ pi 2) (* 4 4/5 pi)))))

(polygon (pentagram-vertices (xy 0 0) 1))
]

@figure[#:tag "fig:pentagrama0"
        #:caption @elem{Creation of a Pentagram.}]{
@(show-tikz 4)
}

It is obvious that the @fn[pentagram-vertices] contains excessive
repetition of code so it would be better if we found a more structured
way of generating those vertices. To do so let us start by thinking
about the generalizing the function.

A general case of a pentagram is the @emph{regular star}, where one varies
the number of vertices and the number of arcs between the connected
vertices. A pentagram is a particular case of a regular star in which
the number of vertices is five and the number of arcs between
connected vertices is two. Mathematically, a regular star is
represented by the Schläfli symbol @footnote{Ludwig Schläfli was a
Swiss mathematician and geometer who made important contributions in
the field of multidimensional geometry.} @${\{\frac{v}{a}\}} where
@${v} is the number of vertices and @${a} is the number of arcs
between the connected vertices. In this notation a pentagram can be
described as @${\{\frac{5}{2}\}}.

In order to draw regular stars let us think of a
function that, given the centre of the star, the radius of the
circumscribed circle, the number of vertices @${v} (which we will call
@fn[n-vertices]) and the number of separation arcs
@${a} (which we will call @fn[n-arcs]), calculates the size of the arc
@${\Delta\phi} between vertices. This arc is obviously
@${\Delta\phi=a\frac{2\pi}{v}}. As in a pentagram, let us consider
for the first vertex an initial angle of
@${\phi=\frac{\pi}{2}}. From that first vertex one needs only successively
increase @${\phi} in increments of @${\Delta\phi}.  The
following functions implement this process:

@def[
(define (star-vertices p radius n-vertices n-arcs)
  (points-circle p 
         	 radius
                 pi/2
                 (* n-arcs (/ 2pi n-vertices))
                 n-vertices))
]

@def[
(define (points-circle p radius fi dfi n)
  (if (= n 0)
    (list)
    (cons (+pol p radius fi)
          (points-circle p 
                         radius 
                         (+ fi dfi) 
                         dfi
                         (- n 1)))))
]

With the @fn[star-vertices] function it is now trivial to generate the
vertices of any regular star. @Figref{fig:estrelas0} presents some
regular stars generated by the following expressions:

@def/no-results[
(polygon (star-vertices (xy 0 0) 1 5 2))
(polygon (star-vertices (xy 2 0) 1 7 2))
(polygon (star-vertices (xy 4 0) 1 7 3))
(polygon (star-vertices (xy 6 0) 1 8 3))
]

@figure[#:tag "fig:estrelas0"
        #:caption @elem{Regular stars. From left to right we have a pentagram (@${\{\frac{5}{2}\}}), two heptagrams (@${\{\frac{7}{2}\}} and @${\{\frac{7}{3}\}}), and one octagram (@${\{\frac{8}{3}\}}).}]{
@(show-tikz 3)}

@Figref{fig:estrelas1} shows a sequence of regular stars
@${\{\frac{20}{r}\}} with @${r} varying between @${1} and @${9}.

@def/no-results[
(for/list ((n (in-range 1 9 1)))
  (polygon (star-vertices (xy (* n 2.5) 0) 1 20 n)))]

@figure[#:tag "fig:estrelas1"
        #:caption @elem{Regular stars @${\{\frac{p}{r}\}} with @${p=20} and @${r} varying between @${1} (on the left) and  @${9} (on the right).}]{
@(show-tikz 1.1)}


It is very important to understand that the construction of stars is
separated into two distinct parts. On one hand, with the function
@fn[star-vertices], we produce the coordinates of the stars' vertices
in the order by which they should be connected. On the other
hand, with the @fn[polygon] function, we use these coordinates to create a
graphical representation of the star based on lines connecting its
vertices. The transition of coordinates from one to the other is
accomplished through a list that is "produced" on one side and
"consumed" on the other.

The use of lists to separate different processes is fundamental
and it will be by us repeatedly explored to simplify our programs.


@subsection[#:tag "sec:poligonosRegulares"]{Regular Polygons}

A @emph{regular polygon} is a polygon that has equal side lengths and
equal angle amplitudes. @Figref{fig:poligonosRegulares} illustrates
examples of regular polygons. Obviously a @emph{regular polygon} is a
particularly case of a regular star in which the number of arcs
between vertices is one.

To create regular polygons, let us define a function called
@fn[regular-polygon-vertices] that generates a list of coordinates
corresponding to the vertices of a regular polygon with @${n} sides,
inscribed in a circle of radius @${r}, centred at @${p}, of which the
"first" vertex makes an angle @${\phi} with the @${X} axis. Being a
regular polygon a particular case of a regular star, we only need to
call the function that computes the vertices of a regular star but using,
for the @${\Delta_\phi} parameter, the value of the
circumference division @${2\pi} by the number of the sides @${n}:

@def[
(define (regular-polygon-vertices n p r fi)
  (points-circle p r fi (/ 2pi n) n))
]

Finally, we can encapsulate the generation of vertices with their use
to create the corresponding polygon:

@def[
(define (regular-polygon n p r fi)
  (polygon
    (regular-polygon-vertices n p r fi)))
]

@def/no-show[
(for/list ((n (in-range 3 11 1)))
  (regular-polygon n (xy (* n 2.5) 0) 1 pi/2))
]

@figure[#:tag "fig:poligonosRegulares"
        #:caption @elem{Regular polygons. From left to right we have a 
equilateral triangle, a square, a regular pentagon, a 
regular hexagon, a regular heptagon, a regular octagon a 
regular enneagon and a regular decagon.}]{
@(show-tikz 1.1)
}

Although we defined them here, the functions
@fn[regular-polygon-vertices] and @fn[regular-polygon] are predefined
in Rosetta.


@questions[
@question{ Consider a polygon represented by a list of its vertices
@lisp[(#,(lispemphi p "0") #,(lispemphi p "1") ... #,(lispemphi p "n"))]
as presented in the diagram on the left:

@fig[@tex{
\begin{tikzpicture}[baseline,scale=1]
\def\polangs{0/0,1/65,2/100,3/170,4/260}
\foreach \i/\a in \polangs { \filldraw(\a:1cm)circle(0.1mm); };
\foreach \i/\a in \polangs { \draw(\a:1.2cm)node{$P_{\i}$}; };
\draw[thick] (0:1cm)\foreach \i/\a in \polangs {--(\a:1cm)}--cycle;
\end{tikzpicture}
$\longrightarrow$
\begin{tikzpicture}[baseline,scale=1]
\def\polangs{0/0,1/65,2/100,3/170,4/260}
\foreach \i/\a in \polangs { \filldraw(\a:1cm)circle(0.1mm); };
\foreach \i/\a in \polangs { \draw(\a:1.2cm)node{$P_{\i}$}; };
\draw[thick] (0:1cm)\foreach \i/\a in \polangs {--(\a:1cm)}--cycle;
\draw[thick] (65:1cm)--(260:1cm);
\end{tikzpicture}
}]

We intend on dividing the polygon into two sub-polygons, as
presented in the previous diagram on the right. Define the function
@fn[divide-polygon] that, given the list of vertices of the
polygon and two indexes @${i} and @${j} that indicate where the
division should be made (with @${i<j}) calculates the lists of
vertices of the resulting sub-polygons and returns them in a list. For
example, in the presented figure we have: @lisp[(divide-polygon
(list #,(lispemphi p "0") #,(lispemphi p "1") #,(lispemphi p "2")
#,(lispemphi p "3") #,(lispemphi p "4")) 1 4)] produces a list of lists
@lisp[((#,(lispemphi p "0") #,(lispemphi p "1") #,(lispemphi p "4"))
(#,(lispemphi p "1") #,(lispemphi p "2") #,(lispemphi p "3")
#,(lispemphi p "4")))] }

@question{ We can consider a generalization of the previous case based
on the @emph{bisection} of the polygon using an arbitrary line, as
presented in the following diagram:

@fig[@tex{
\begin{tikzpicture}[baseline,scale=1]
\def\polangs{0/0,1/65,2/100,3/170,4/260}
\foreach \i/\a in \polangs { \filldraw(\a:1cm)circle(0.1mm); };
\foreach \i/\a in \polangs { \draw(\a:1.2cm)node{$P_{\i}$}; };
\draw[thick] (0:1cm)\foreach \i/\a in \polangs {--(\a:1cm)}--cycle;
\end{tikzpicture}
$\longrightarrow$
\begin{tikzpicture}[baseline,scale=1]
\def\polangs{0/0,1/65,2/100,3/170,4/260}
\foreach \i/\a in \polangs { \filldraw(\a:1cm)circle(0.1mm); };
\foreach \i/\a in \polangs { \draw(\a:1.2cm)node{$P_{\i}$}; };
\draw[thick] (0:1cm)\foreach \i/\a in \polangs {--(\a:1cm)}--cycle;
\draw[thick] ($(65:1cm)!.3!(100:1cm)$)--($(260:1cm)!.5!(0:1cm)$);
\end{tikzpicture}
}]

To simplify this process, consider that each end of the
bisection line is located at a certain @${f_i\in [0,1]} fraction of the
distance that goes from the vertex @${P_i} to the next vertex @${P_{i+1}}
(with @${P_{n+1}=P_0}). For example, in the previous diagram, the vertices
in question are @${P_1} and @${P_4} and the fractions respectively
@${f_1=0.3} and @${f_4=0.5}.

Define the function @fn[polygon-bisection] that, given the list of
vertexes of the polygon and the two indexes @${i} and
@${j}immediately prior to the limits of the bisection line, and given
the distance fractions (@${f_i} and @${f_j}), respectively between
vertices @${(P_i,P_{i+1})} and @${(P_j, P_{j+1})}, calculates the
lists of vertexes of each of the resulting sub-polygons and returns
them in a list (i.e., returns a list of a list of vertices).  }


@question{Define the function @fn[random-polygon-bisection] that,
given the polygon vertices, returns a list of lists of vertices of the
sub-polygon corresponding to a random division of the polygon. The
following image shows examples of random divisions of an octagon.

@def/no-show[
(define (ponto-intermedio p0 p1 f)
  (p+v p0 (v*r (p-p p1 p0) f)))

(define (bisseccao-poligono pts i j fi fj)
  (let ((pti (ponto-intermedio (list-ref pts i) (list-ref pts (+ i 1)) fi))
        (ptj (ponto-intermedio (list-ref pts j) (list-ref (append pts pts) (+ j 1)) fj)))
    (list (append (sublist pts 0 (+ i 1))
                  (list pti ptj)
                  (sublist pts (+ j 1) (length pts)))
          (append (list ptj pti)
                  (sublist pts (+ i 1) (+ j 1))))))

(define (bisseccao-aleatoria-poligono pts)
  (let* ((l (length pts))
         (ij (sequence->list (in-range 0 l 1)))
         (i (list-ref ij (random l)))
         (j (list-ref (remove i ij) (random (- l 1)))))
    (bisseccao-poligono pts
                        (min i j)
                        (max i j)
                        (random-range 0.0 1.0)
                        (random-range 0.0 1.0))))

(random-seed! 12343)
(for/list ((i (in-range 0 11 1)))
  (for/list ((polig
              (bisseccao-aleatoria-poligono
               (regular-polygon-vertices 10 (xy (* i 2.5) 0) 1 0))))
    (polygon polig)))
]

@fig[@(show-tikz 0.9)]
}

@question{Using the @fn[random-polygon-bisection] function defined
in the previous exercise, define the function @fn[random-polygon-division]
that randomly and recursively divides a polygon until a certain recursion
level is achieved. The following image shows
the random division of a decagon from @${0} to @${10} levels of
recursion.

@def/no-show[
(define (divisao-aleatoria-poligonos pols n)
  (if (null? pols)
    (list)
    (append (divisao-aleatoria-poligono (car pols) n)
            (divisao-aleatoria-poligonos (cdr pols) n))))

(define (divisao-aleatoria-poligono pts n)
  (if (= n 0)
    (list pts)
    (divisao-aleatoria-poligonos
     (bisseccao-aleatoria-poligono pts)
     (- n 1))))

(for/list ((i (in-range 0 11 1)))
  (random-seed! 12344)
  (for/list ((polig
              (divisao-aleatoria-poligono
               (regular-polygon-vertices 10 (xy (* i 2.5) 0) 1 0)
               i)))
    (polygon polig)))
]

@fig[@(show-tikz 0.9)]
}
]

@section[#:tag "sec:plineVsSpline"]{Polygonal Lines and @emph{Splines}}

We have seen that lists allow the storage of a variable number of elements and
we have seen that is possible to use lists to separate programs that
define the vertices of the geometric entities from the ones that use
these vertices to represent them graphically.

In the case of @fn[polygon] function discussed in the section
@ref{sec:polygons}, the lists of vertices are used to create polygons,
i.e., flat figures limited by a closed path composed of a sequence of
line segments. It so happens that we do not always want to create figures
limited by closed paths, or that these paths are sequences of line
segments, not even flat figures. To solve this problem we
need to use functions capable of, given lists of positions,
creating other types of geometric figures.

The simplest case is a polygonal line, not necessarily planar,
that if open, can be created by the @fn[line] function, and if
closed, can be created by the @fn[closed-line] function. In either case,
these functions accept a variable number of arguments
corresponding to the vertices of the polygonal line or a list of these
vertices.

In case we do not want polygonal lines but "smooth" curves that pass through
a sequence of positions, we can use the @fn[spline] function to
create open curves and the @fn[closed-spline] function to create
closed curves. As the @fn[line] and @fn[closed-line] functions,
these also accept a variable number of arguments or a list of these
arguments.

The difference between a polygonal line and a @emph{spline} is
visible in @figref{fig:plineVsSpline}, in which we compare a
sequence of points linked with a polygonal line and the same sequence
linked with a @emph{spline}. The image was produced by evaluating
the following expressions:

@def[
(define points
        (list (xy 0 2) (xy 1 4) (xy 2 0) (xy 3 3)
              (xy 4 0) (xy 5 4) (xy 6 0) (xy 2 7)
              (xy 8 1) (xy 9 4)))

(line points)
(spline points)
]

@figure[#:tag "fig:plineVsSpline"
	#:caption @elem{Comparison between a polygonal line and a @emph{spline} joining the same set of points.}]{
@tex{
\begin{tikzpicture}[scale=0.6]
    \inputtikz{plineVsSpline}
  \end{tikzpicture}}}

Naturally, the "manual" specification of point coordinates is
rather inconvenient, and it is preferable that those coordinates be
automatically computed according to a mathematical definition of the
desired curve. For example, imagine that we intend to draw a sine
wave starting at a point @${P}. Unfortunately, in the list of
geometrical figures provided by Rosetta (points, lines, line segments,
rectangles, polygons, circles, arcs, etc), the sinusoid is not included.

In order to solve this problem we can create an approximation to a
sinusoid curve. To do so we can calculate a sequence of
points that belong to the sinusoid curve and join them using lines,
or better yet, using curves that pass through those points. To calculate
the set of values of the sine
function in the interval @${[x_0, x_1]} we need only consider an increment
${\Delta_x} and, starting at point @${x_0}, and going from point
@${x_i} to point @${x_ {i +1}} using @${x_{i+1}=x_i+\Delta_x},
we will be successively calculating the value of  expression @${\sin(x_i)}
until @${x_i} exceeds @${x_1}. To get a recursive definition for this
problem we can think that when @${x_0>x_1} the result will
be an empty list of coordinates, otherwise we add the coordinate
@${(x_0,\sin(x_0))} to the list of sine coordinates for the
interval @${[x_0+\Delta_x, x_1]}. This definition is directly translated to the
following function:

@def[
(define (sine-points x0 x1 dx)
  (if (> x0 x1)
    (list)
    (cons (xy x0 (x0 sin))
          (sine-points (x0 + dx) dx x1))))
]

To have greater freedom in the positioning of the sinusoid
curve in space, we can modify the previous function to incorporate a
point @${p} in relation to which the curve is positioned:

@def[
(define (sine-points p x0 x1 dx)
  (if (> x0 x1)
    (list)
    (cons (+y p (sin x0))
          (sine-points (+x p dx) (+ x0 dx) x1 dx))))
]

@figref{fig:senosPline} shows the curves traced by the
following expressions in which the points are joined through polygonal
lines:

@(clear-tikz)

@def[
(line (sine-points (xy 0.0 1.0) 0.0 6.0 1.0))
(line (sine-points (xy 0.0 0.5) 0.0 6.5 0.5))
(line (sine-points (xy 0.0 0.0) 0.0 6.4 0.2))
]

@figure[#:tag "fig:senosPline"
	#:caption @elem{Sinusoid curves drawn using @emph{plines} with an increasing number of points.}]{
@(show-tikz 3)}

Note that, in @figref{fig:senosPline}, we gave a vertical offset to
the curves to better understand the difference in accuracy between
them. It is plainly evident that the more points used to calculate the
curve, the more the
polygonal line resembles the true curve. However, we must also take into
account that increasing the number of points also forces Rosetta (and the
CAD tool) to a greater computational effort.

For even better approximations, although at the cost of even greater
computational effort, we can use @emph{splines}, simply by changing
the previous expressions to use the @fn[spline] function instead of the
@fn[line] function. The result is presented in @figref{fig:senosSpline}.

@def/no-show[
(spline (sine-points (xy 0.0 1.0) 0.0 6.0 1.0))
(spline (sine-points (xy 0.0 0.5) 0.0 6.5 0.5))
(spline (sine-points (xy 0.0 0.0) 0.0 6.4 0.2))
]

@figure[#:tag "fig:senosSpline"
        #:caption @elem{Sinusoid curves drawn using @emph{splines} with an increasing number of points.}]{
@(show-tikz 3)}

@questions[
@question{ Define the @fn[sinusoid-circular-points] function, with
parameters @${p}, @${r_i}, @${r_e}, @${c} and @${n} that computes
@${n} points of a closed curve in the shape of a sinusoid with @${c}
cycles that develops along a circular ring centred at the point @${p}
and enclosed by the inner radius @${r_i}  outer radius @${r_e}, as can
be seen in the various examples presented in the following
figure where, from left to right, the number of @${c} cycles is 12, 6 and 3.

@fig[@tex{
    \sinusCircularDraw{12345}{0,2,...,358}{12} \hfill
    \sinusCircularDraw{12342}{0,3,...,357}{6} \hfill
    \sinusCircularDraw{12343}{0,5,...,355}{3}
}]
}

@question[#:tag "question:pontosCirculoRaioAleatorio"]{ Define the
@fn[random-radius-circle-points] function, with parameters @${p}, @${r_0},
@${r_1} and @${n} that computes @${n} points of a closed random curve
developing along a round circle centred at point @${p} and enclosed by the
inner radius @${r_0} and outer radius @${r_1}, as can be seen in the various
examples presented in the following figure where, from left to right, the
number of points used gradually increased, thereby increasing the curve's
irregularity.

@fig[@tex{
\randomShape{12345}{0,90,...,300}
\hfill
\randomShape{12342}{0,60,...,330}
\hfill
\randomShape{12343}{0,30,...,345}
\hfill
\randomShape{12344}{0,15,...,345}
}]

Tip: for computing the points, consider using polar coordinates to evenly
distribute the points around a circle but with the distance to the
centre varying randomly between @${r_0} and @${r_1}. For example,
consider that the leftmost curve in the previous figure was generated
by the following expression:

@lispcode[
(closed-spline
  (points-circle-radius-random
    (xy 0 0) 1 2 6))
]}]

@section[#:tag "sec:trelicas"]{Trusses}

A truss is a structure composed by rigid bars that join together in
nodes, forming triangular units. The triangle being the only polygon
intrinsically stable, the use of triangles conveniently interconnected
allows the trusses to be undeformable structures. Despite the
simplicity of triangular elements, different arrangements of these
elements allow for different types of trusses.


@figure[#:tag "fig:bucky" #:caption @elem{The Buckminster Fuller's
geodesic sphere. Photograph by Glen Fraser.}]{
@authorizedPhoto{geodesicas/GlenFraser}}

The use of trusses is known since ancient Greece, where they were
used to support roofs. In the sixteenth century, in his @emph{Four
Books of Architecture}, Andrea Palladio illustrates bridges made of
trusses. In the nineteenth century, with the extensive use of metal and
the need to overcome increasing spans, various types of trusses were
invented that were distinct in the different arrangements of vertical,
horizontal and diagonal struts, and they are frequently denominated
according with their inventors. Thus we have Pratt's trusses, Howe's
trusses, Town's trusses, Warren's trusses, etc. In the last decades
trusses began to be intensively used as an artistic element or for the
construction of elaborate surfaces. The set of most famous
examples includes Buckminster Fuller's geodesic sphere for the
Universal Exhibition of 1967, showed (reconstructed) in
@figref{fig:bucky} and the banana shaped trusses by Nicolas Grimshaw
for the Waterloo terminal, shown in @figref{fig:grimshaw}.

@figure[#:tag "fig:grimshaw"
        #:caption @elem{Trusses shaped in banana shape for the Waterloo terminal, by Nicolas Grimshaw. Photograph by Thomas Hayes.}]{
  @authorizedPhoto{trelicas/ThomasHayes}}

Trusses show a set of properties that make them particularly
interesting from an architectural point of view:

@itemize[

@item{It is possible to build very large trusses from relatively small
elements, facilitating the production, transport and erection.}

@item{Provided that the weight is applied only on the truss' nodes, the
struts are only subject to axial forces, i.e., they work only by
traction or compression, allowing structural forms of great
efficiency.}

@item{As the basic elements of construction are struts and nodes, it is
easy to adapt its dimensions to the expected loads, thus allowing
great flexibility.}  ]

@subsection{Drawing of Trusses}

A fundamental step for drawing trusses is the design and construction
of the fundamental triangular elements. Although it is common to only
consider bi-dimensional trusses (also known as @emph{plane trusses}), we
will study the standard case of three-dimensional trusses
composed by semi-octahedron. This type of truss is called @emph{space
frame}. Each semi-octahedron is called @emph{module}.

@Figref{fig:trelica0} presents a diagram of a truss. Although the
nodes in this truss are equally spaced along the parallel lines, it
needs not be that way necessarily. @Figref{fig:trelica1}
shows a different truss in which that does not happen.


@figure[#:tag "fig:trelica0"
        #:caption @elem{Trusses composed by equal triangular elements.}]{
@tex{
\begin{tikzpicture}[scale=2]
\inputtikz{trelicaA}
\end{tikzpicture}}}

@figure[#:tag "fig:trelica1"
        #:caption @elem{Trusses in which the triangular elements are not equal amongst each other.}]{
@tex{
\begin{tikzpicture}[scale=2]
\inputtikz{trelicaB}
\end{tikzpicture}}}

Therefore, for drawing a truss, let us consider, as working base,
three arbitrary sequences of points in which each point defines one
truss node. From these three sequences we can create the connections
that need to be established between each pair of nodes.
@Figref{fig:trelica2} shows the connection scheme of three
sequences of points @${a_0,a_1, a_2}, @${b_0, b_1)} and @${c_0, c_1,
c_2)}. It should be noted that the top sequence established by the
@${b_i} points of the intermediate sequence always has one less
element than the @${a_i0} and @${c_i} sequences.


@figure[#:tag "fig:trelica2"
        #:caption @elem{Strut connection scheme of a truss in @emph{space
    frame}.}]{
@tex{
\begin{tikzpicture}[scale=2]
\inputtikz{trelica}
\end{tikzpicture}}}

For the truss construction we need to find a process that, from the
lists of points @${a_i}, @${b_i} and @${c_i}, not only creates the
corresponding nodes for the various points, but also interconnects them
in the correct way. Let us deal first with the nodes creation:

@def[
(define (truss-nodes ps)
  (if (null? ps)
    #t
    (begin
      (truss-node (car ps))
      (truss-nodes (cdr ps)))))
]


The @fn[truss-node] function (note the use of the singular, by
opposing the plural used in the @fn[truss-nodes] function) receives
the coordinates of a point and it is responsible for creating
the three-dimensional model that represents the truss node centred in
that point. One simple hypothesis will be for this function to create
a sphere where the struts will be connected to, but, for now, let us leave
the decision of what that model should be for latter and let us simply
admit that the function @fn[truss-node] will execute something
appropriate to create the node. Thus, we can start idealizing the
function that builds a complete truss from the following lists of
points @lisp[as], @lisp[bs] e @lisp[cs]:

@lispcode[
(define (truss as bs cs)
  (truss-nodes as)
  (truss-nodes bs)
  (truss-nodes cs)
  ...)
]

After that, let us begin to establish the struts in between the nodes. From
analysing @figref{fig:trelica2} we know that we have one
connection between each @${a_i} and each @${c_i}, another between each
@${a_i} and @${b_i}, another between @${c_i} and @${b_i}, another
between @${b_i} and @${a_{i+1}}, another between @${b_i} and
@${c_{i+1}}, another between @${a_i} and @${a_{i+1}}, another between
@${b_i} and @${b_{i+1}} and finally, another between @${c_i} and @${c_{i+1}}.
Admitting that the @fn[truss-strut] function creates
the three-dimensional model of that strut (for example, a cylinder, or a
prismatic strut), we can start by defining a function denominated
@fn[truss-struts] (plural) that, given two lists of
points @lisp[ps] and @lisp[qs], creates connection struts along
successive pairs of points. To create a strut, the function
needs one element from @lisp[ps] and another from @lisp[qs], which
implies that the function should end as soon as one of these lists is
empty. Thus the definition is:


@def[
(define (truss-struts ps qs)
  (if (or (null? ps) (null? qs))
    #t
    (begin
      (truss-strut (car ps) (car qs))
      (truss-struts (cdr ps) (cdr qs)))))
]

In order to interconnect each @${a_i} node to the corresponding
@${c_i} node, we only need to evaluate @lisp[(truss-struts as cs)]. The
same can be said for interconnecting each node @${b_i} to the
corresponding node @${a_i} and to interconnect each @${b_i} to each
@${c_i}. Thus, we have:

@lispcode[
(define (truss as bs cs)
  (truss-nodes as)
  (truss-nodes bs)
  (truss-nodes cs)
  (truss-struts as cs)
  (truss-struts bs as)
  (truss-struts bs cs)
  ...)
]

To connect the @${b_i} nodes to the @${a_{i+1}} nodes we can simply
subtract the first node from the list @lisp[as] and establish the
connection as before. The same can be done to
connect each @${b_i} to each @${c_{i+1}}. Finally, to connect each
@${a_i} to each @${a_{i+1}} we can use the same idea but applying it
only to the list @lisp[as]. The same can be done to the list
@lisp[cs]. The complete function will thus be:

@def[
(define (truss as bs cs)
  (truss-nodes as)
  (truss-nodes bs)
  (truss-nodes cs)
  (truss-struts as cs)
  (truss-struts bs as)
  (truss-struts bs cs)
  (truss-struts bs (cdr as))
  (truss-struts bs (cdr cs))
  (truss-struts (cdr as) as)
  (truss-struts (cdr cs) cs)
  (truss-struts (cdr bs) bs))
]

The previous functions creates trusses based on the "elemental" functions
@fn[truss-node] and @fn[truss-strut]. Although their
meaning is obvious, we have not yet defined these functions and there
are several possibilities. For a first approach let us consider that
every truss node will be formed by a sphere to which the struts will connect,
with these struts defined by cylinders. The spheres and
cylinders radii will be determined by a global variable, so that
we can easily change its value. Thus, we have:

@def[
(define truss-node-radius 0.1)

(define (truss-node p)
  (sphere p truss-node-radius))

(define truss-strut-radius 0.03)

(define (truss-strut p0 p1)
  (cylinder p0 truss-strut-radius p1))
]

We can now create trusses with any form we want.
@Figref{fig:trelica2} shows a truss drawn from the expression:

@figure[#:tag "fig:trelica2"
        #:caption @elem{Truss build from arbitrary specified points.}]{
@autoimage{trelica}}

@lispcode[
(truss
 (list (xyz 0 -1 0) (xyz 1 -1.1 0) (xyz 2 -1.4 0) (xyz 3 -1.6 0)
       (xyz 4 -1.5 0) (xyz 5 -1.3 0) (xyz 6 -1.1 0) (xyz 7 -1 0))
 (list (xyz 0.5 0 0.5) (xyz 1.5 0 1) (xyz 2.5 0 1.5) (xyz 3.5 0 2) 
       (xyz 4.5 0 1.5) (xyz 5.5 0 1.1) (xyz 6.5 0 0.8))
 (list (xyz 0 +1 0) (xyz 1 +1.1 0) (xyz 2 +1.4 0) (xyz 3 +1.6 0) 
       (xyz 4 +1.5 0) (xyz 5 +1.3 0) (xyz 6 +1.1 0) (xyz 7 +1 0)))
]

@question{ Define a @fn[straight-truss] function capable of building
any of the trusses shown in the following image.

@fig[@tex{
\begin{tikzpicture}[scale=0.6]
\inputtikz{trelicaRecta}
\end{tikzpicture}
}]

To simplify, consider that the trusses develop along the @${X}
axis. The @fn[straight-truss] function should receive the initial truss
point, the height and width of the truss and the number of nodes of
the lateral rows. With these values, the function should produce three
lists of coordinates which will be given as arguments to the @fn[truss]
function. As an example, consider that the three trusses presented on
the previous image were the result of the evaluation of the following
expressions:

@def/no-show[
(define (line-coordinates p l n)
  (if (= n 0)
    (list)
    (cons p
          (line-coordinates (+x p l) l (- n 1)))))

(define (line-truss p h l n)
  (truss
   (line-coordinates p l n)
   (line-coordinates (+xyz p (/ l 2) (/ l 2) h) l (- n 1))
   (line-coordinates (+y p l) l n)))
]

@lispcode[
(line-truss (xyz 0  0 0) 1.0 1.0 20)
(line-truss (xyz 0  5 0) 2.0 1.0 20)
(line-truss (xyz 0 10 0) 1.0 2.0 10)
]

Suggestion: begin by defining a @fn[line-coordinates] function
that, given an initial pointy @${p}, a separation between points @${l}
and a @${n} number of points, returns a list with the coordinates of
the @${n} points arranged along the @${X} axis.  }

@question{ The total cost of a truss depends heavily on the number of
different lengths that the struts may have: the smaller that number is,
greater economies of scale can be obtained and, consequently, the
cheaper the truss will be. The ideal case is when all strut lengths
are the same.
 
Given the following scheme, determine the height @${h} of the truss
in terms of the width @${l} of the @emph{module} so that all the
struts have the same length.
  
@fig[@tex{
\begin{tikzpicture}[scale=0.5]
\inputtikz{trelicaModulo}
\end{tikzpicture}
}]

Define also the @fn[truss-module] function that builds a truss
with all the struts having the same length, orientated along the @${X}
axis. The function should receive the truss initial point, its width
and the number of nodes of the lateral rows.  }

@question{ Consider the drawing of a @emph{plane} truss, as shown
in the following figure:

@fig[@tex{
\begin{tikzpicture}
\filldraw[thick](0,0)circle(0.3mm)--(1,1)circle(0.3mm)--(2,0)circle(0.3mm)--(3,1)circle(0.3mm)--(4,0)circle(0.3mm)--(5,1)circle(0.3mm)--(6,0)circle(0.3mm)--(7,1)circle(0.3mm)--(8,0)circle(0.3mm);
\filldraw[thick](0,0)node[below]{$a_0$}--(2,0)node[below]{$a_1$}--(4,0)node[below]{$\ldots$}--(6,0)node[below]{$a_{n-1}$}--(8,0)node[below]{$a_n$};
\filldraw[thick](1,1)node[above]{$b_0$}--(3,1)node[above]{$b_1$}--(5,1)node[above]{$\ldots$}--(7,1)node[above]{$b_{n-1}$};
\end{tikzpicture}
}]

Define a @fn[plane-truss] function that receives, as parameters,
two lists of points corresponding to the points from @${a_0} to
@${a_n} and from @${b_0} to @${b_{n-1}} and that creates the nodes in
those points and the struts that unite them. Consider as pre-defined,
the functions @fn[truss-nodes], that receives one list of points as
argument, and the @fn[truss-struts] function that receives two lists
of points as arguments.

@def/no-show[
(define (plane-truss as bs)
  (truss-nodes as)
  (truss-nodes bs)
  (truss-struts as bs)
  (truss-struts bs (cdr as))
  (truss-struts as (cdr as))
  (truss-struts bs (cdr bs)))
]

Test the function with the following expression: 

@lispcode[
(plane-truss
  (line-coordinates (xyz 0 0 0) 2.0 20)
  (line-coordinates (xyz 1 0 1) 2.0 19))
]
}

@question{
Consider the special truss represented in the following figure:


@fig[@tex{
\begin{tikzpicture}[scale=0.6]
\filldraw[thick] (1.324,9.316)--(4.101,10.019);
\filldraw[thick] (1.324,9.316)--(6.336,7.767);
\filldraw[thick] (4.101,10.019)--(9.104,8.469);
\filldraw[thick] (5.218,5.311)--(2.712,9.672);
\filldraw[thick] (5.218,5.311)--(7.724,8.122);
\filldraw[thick] (5.218,5.311)--(10.231,3.762);
\filldraw[thick] (6.336,7.767)--(9.104,8.469);
\filldraw[thick] (6.336,7.767)--(11.348,6.217);
\filldraw[thick] (9.104,8.469)--(14.117,6.92);
\filldraw[thick] (10.231,3.762)--(7.724,8.122);
\filldraw[thick] (10.231,3.762)--(12.737,6.573);
\filldraw[thick] (10.231,3.762)--(15.234,2.213);
\filldraw[thick] (11.348,6.217)--(14.117,6.92);
\filldraw[thick] (11.348,6.217)--(16.36,4.676);
\filldraw[thick] (14.117,6.92)--(19.129,5.371);
\filldraw[thick] (15.234,2.213)--(12.737,6.573);
\filldraw[thick] (15.234,2.213)--(17.74,5.024);
\filldraw[thick] (16.36,4.676)--(19.129,5.371);

\filldraw[thick] (1.324,9.316)circle(1mm)node[above]{$a_0$};
\filldraw[thick] (10.231,3.762)circle(1mm);
\filldraw[thick] (11.348,6.217)circle(1mm);
\filldraw[thick] (12.737,6.573)circle(1mm);
\filldraw[thick] (14.117,6.92)circle(1mm);
\filldraw[thick] (15.234,2.213)circle(1mm)node[below]{$b_{n-1}$};
\filldraw[thick] (16.36,4.676)circle(1mm)node[above]{$a_n$};
\filldraw[thick] (17.74,5.024)circle(1mm);
\filldraw[thick] (19.129,5.371)circle(1mm)node[above]{$c_n$};
\filldraw[thick] (2.712,9.672)circle(1mm);
\filldraw[thick] (4.101,10.019)circle(1mm)node[above]{$c_0$};
\filldraw[thick] (5.218,5.311)circle(1mm)node[below]{$b_0$};
\filldraw[thick] (6.336,7.767)circle(1mm)node[above]{$a_1$};
\filldraw[thick] (7.724,8.122)circle(1mm);
\filldraw[thick] (9.104,8.469)circle(1mm)node[above]{$c_1$};
\end{tikzpicture}
}]

Define a @fn[special-truss] function that receives as parameters
three lists of points corresponding to the points from @${a_0} to
@${a_n}, from @${b_0} to @${b_{n-1}} and from @${c_0} to @${c_n} and
creates the nodes at those points and the struts that unite
them. Consider as pre-defined the functions @fn[nodes-truss], that
receives one list of points as argument, and @fn[struts-truss]
function that receives two lists of points as argument.  }


@subsection{Creating Positions}

As we saw in the previous section, it is possible to idealize process
of creating a truss from the lists of the node
positions. Naturally, these lists, can be specified manually but this
approach will only be viable for very small trusses. However, a truss
being a structure capable of covering very large spans, in general,
the number truss nodes is too high for us to manually produce the list of
positions. To solve this problem we need to think in automated
processes to create these lists. These processes also have to
take into account the desired geometry for the truss.

As an example, let us idealize the creation process for a truss in the
shape of an arc, wherein the node sequences @${a_i}, @${b_i} and
@${c_i} form circumference arcs. @Figref{fig:trelicaArco} shows
one version of one such truss, defined by the circumference arcs
of radius @${r_0} and @${r_1}.

@figure[#:tag "fig:trelicaArco"
        #:caption @elem{Front elevation of a truss in shape of a circular arc.}]{
@tex{
\begin{tikzpicture}[scale=1.9]
\inputtikz{esquemaTrelicaArco}
\end{tikzpicture}}}

To make the truss uniform, the nodes are equally spaced along the arc.
The angle @${\Delta_\psi} corresponds to that spacing and its
trivially calculated by dividing the arc angular amplitude by the number
@${n} of nodes required. Considering that the intermediate arc always has
one less node than the lateral arcs, we need to divide the
@${\Delta_\psi} angle by the two extremities of the intermediate arc,
in order to centre these arc's nodes in between the nodes of the
lateral arcs, as it is possible to see in @figref{fig:trelicaArco}.

Since the arc is circular, the most simple way of calculating the node
positions will be to use spherical coordinates @${\rho,
\phi,\psi)}. This decision leads us to consider that the initial and
final arcs angles should be measured relative to the @${Z} axis, as
visible in @figref{fig:trelicaArco}. In order to make the production of
node coordinates of each arc more flexible let us define a
function that receives the centre @${P} of the arc, the radius @${r} of
that arc, the angle @${\phi}, the initial @${\psi_0} and final @${\psi_1}
angles and finally, the angle increment @${\Delta_\psi}. Thus, we have:


@def[
(define (arc-points p r fi psi0 psi1 dpsi)
  (if (> psi0 psi1)
    (list)
    (cons (+sph p r fi psi0)
          (arc-points p r fi (+ psi0 dpsi) psi1 dpsi))))
]

To build the arc truss we can now define a function that creates three
of the previous arcs. For this, the function will receive the centre @${p}
of the central arc, the radius @${r_{ac}} of the lateral arcs,
the radius @${r_b} of the central arc, the angle @${\phi}, the initial
@${\psi_0} and the final @${\psi_1} angles and also, the
separation @${e} between the lateral arcs and the number @${n} of nodes of
the lateral arcs. The function will calculate the increment
@${\Delta_\psi=\frac{\psi_1-\psi_0}{n}} and then call the
@fn[truss] function with the appropriate parameters:

@def[
(define (arc-truss p rac rb fi psi0 psi1 e n)
  (define dpsi (/ (- psi1 psi0) n))
  (truss
    (arc-points (+pol p (/ e 2.0) (+ fi pi/2))
                 rac 
                 fi
                 psi0 psi1 
                 dpsi)
    (arc-points p 
                 rb 
                 fi
                 (+ psi0 (/ dpsi 2.0)) (- psi1 (/ dpsi 2.0))
                 dpsi)
    (arc-points (+pol p (/ e 2.0) (- fi pi/2))
                 rac
                 fi
                 psi0 psi1
                 dpsi)))]

@Figref{fig:trelicasArcoRender} shows the trusses built from the
expressions:

@lispcode[
(arc-truss (xyz 0 0 0) 10 9 0 -pi/2 pi/2 1.0 20)
(arc-truss (xyz 0 5 0)  8 9 0 -pi/3 pi/3 2.0 20)
]

@figure[#:tag "fig:trelicasArcoRender"
        #:caption @elem{Arc trusses created with different parameters.}]{
@tex{
\begin{tikzpicture}
\inputtikz{trelicaArco}
\end{tikzpicture}
}}


@question{ Consider the construction of vaults supported on trusses
radially distributed, such as the one displayed in the following
image:
 

@fig[@tex{
\begin{tikzpicture}
\inputtikz{trelicaArcoRadial}
\end{tikzpicture}
}]

This vault is composed by a determined number of circular arc
trusses. The width @${e} of each truss and the initial angle @${\psi_0}
with which each truss starts are such that the nodes at the top of the
trusses are coincidental in pairs and are arranged along a circle of
radius @${r}, as shown in the following scheme:

@fig[@tex{
\begin{tikzpicture}
\inputtikz{esquemaTrelicaArcoRadial}
\end{tikzpicture}
}]

Define the @fn[truss-vault] function that builds a vault of trusses
from the vault centre point @${p}, the radius @${r_{ac}} of the
lateral arc of each truss, the radius @${r_b} of the central arc of
each truss, the radius @${r} of the "end" of the trusses, the @${n}
number of nodes in each truss and, finally, the @${n_\phi} number of
trusses.

As an example, consider the following figure that was produced by the
evaluation of the following expressions:


@lispcode[
(truss-vault (xyz  0 0 0) 10 9 2.0 10 3)
(truss-vault (xyz 25 0 0) 10 9 2.0 10 6)
(truss-vault (xyz 50 0 0) 10 9 2.0 10 9)
]

@fig[@tex{
\begin{tikzpicture}[scale=0.6]
\inputtikz{trelicaArcoRadialB}
\end{tikzpicture}
}]

@question{ 
Consider the creation of a step ladder identical to the one shown on the following figure:
  

@fig[@autoimage[#:scale 0.15]{escadaTrelica}]

Note that the step ladder can be seen as a (very) simplified version
of a truss composed only by two sequences of nodes. Define the
@fn[step-ladder] function that receives as parameters two lists
of points corresponding to the centre of the nodes sequences and that
creates the nodes at those points and the struts that join
them. Consider as pre-defined the function @fn[truss-node], that
receive one point as argument, and the function @fn[strut-truss], that
receives two points as arguments.  }

@question{ Define a function capable of representing the double genome
helix, as shown in the following image:
 
@fig[@autoimage[#:scale 0.75]{genoma}]
}

The function should receive a position referring to the centre of the
genome base, the radius of the genome helix, the angular difference
between the nodes, the height difference between the nodes and,
finally, the number of nodes in each helix. The genomes represented in
the previous image were created by the following expressions:


@lispcode[
(genome (xyz 0 0 0) 1.0 (/ pi 32) 0.5 20)
(genome (xyz 4 0 0) 1.0 (/ pi 16) 0.25 40)
(genome (xyz 8 0 0) 1.0 (/ pi 8) 0.125 80)
]}


@subsection[#:tag "sec:trelicaEspacial"]{Spatial Trusses}

We have seen how it is possible to define trusses from three lists
each containing the nodes' coordinates to which the struts are
connected. Connecting these various trusses with each other it is
possible to create an even bigger structure that is denominated as
@emph{spatial truss}. @Figref{fig:alAin} shows an example where
three spatial trusses are visible.


@figure[#:tag "fig:alAin"
        #:caption @elem{Spatial trusses in the AL Ain Stadium in the United Arabs Emirates. Photograph by Klaus Knebel.}]{
  @authorizedPhoto{trelicas/Al_Ain_Stadium}
}

In order to define an algorithm that generates spatial trusses it is
important to take into account that although these type of trusses
groups various simple trusses, these are interconnected in such a
way that each truss shares a set of nodes and struts with the adjacent
truss, as shown in @figref{fig:trelicaLigada}, where a
a spatial truss scheme is presented. This way, if a spatial truss is
composed by two simple interconnected trusses, the spatial truss will be
generated, not by six lists of coordinates, but only by five lists of
coordinates. In the general case, a spatial truss formed by @${n} simple
trusses will be defined by @${2n+1} lists of points, i.e., by an uneven
number of lists of points (minimum, by three lists).


@figure[#:tag "fig:trelicaLigada"
        #:caption @elem{Strut connection diagram of a spatial truss.}]{
@tex{
\begin{tikzpicture}[scale=3]
\inputtikz{esquemaTrelicaEspacial}
\end{tikzpicture}
}}

The definition of the function that creates a spatial truss follows
the same logic for building a simple truss only
now, instead of operating with only three lists, it operates with an
uneven number of lists. Thus, from a list containing an uneven number
of lists of coordinates, we will process these lists of coordinates two by
two, knowing that the "third" list of coordinates @${c_{i,j}} of
truss @${i} is also the "first" list of coordinates @${a_{i+1,j}} of the
following list @${i+1}.

Seeing as we process two lists at a time and start with an uneven number of
lists, in the "final" case there will be only one list of coordinates left
needed to "close" the creation of the truss.

There is, however an additional difficulty: for the plain truss to
have transversal rigidity it is still necessary to interconnect
the central nodes of the various trusses amongst each other. This
transversal rigidity correspond to connecting each @${b_{i,j}} node to
the node @${b_{i+1,j}}. Thus, as we process the lists of coordinates, we
will also establish the transversal rigidity between the corresponding
lists. The entire process is implemented by the following function:


@def[
(define (spatial-truss curves)
  (let ((as (car curves))
        (bs (cadr curves))
        (cs (caddr curves)))
    (nodes-truss as)
    (nodes-truss bs)
    (struts-truss as cs)
    (struts-truss bs as)
    (struts-truss bs cs)
    (struts-truss bs (cdr as))
    (struts-truss bs (cdr cs))
    (struts-truss (cdr as) as)
    (struts-truss (cdr bs) bs)
    (if (null? (cdddr curves))
        (begin
          (nodes-truss cs)
          (struts-truss (cdr cs) cs))
        (begin
          (struts-truss bs (cadddr curves))
          (spatial-truss (cddr curves))))))
]


@question{ In reality, a simple truss is a particular case of a
spatial truss. Redefine the @fn[truss] function in a way that it
uses the @fn[spatial-truss] function.
}
 
Now that we know how to build spatial trusses from a list of
lists of coordinates, we can think of mechanisms to generate this list
of lists. A simple example is a horizontal spatial truss, as the one
presented in the @figref{fig:trelicaEspacialHorizontal}.

@figure[#:tag "fig:trelicaEspacialHorizontal"
        #:caption @elem{An horizontal spatial truss composed of eight simple trusses with ten pyramids each.}]{
@tex{
  \begin{tikzpicture}[scale=0.7]
\inputtikz{trelicaEspacialPlana0}
\end{tikzpicture}}}

To generate the coordinates of the nodes we can define a
function that, based on the number of desired and on the width
of the pyramid base, generates the nodes along one of the dimensions,
for example, the @${X} dimension:

@def[
(define (line-coordinates p l n)
  (if (= n 0)
    (list)
    (cons p
          (line-coordinates (+x p l) l (- n 1)))))
]

Next, we only have to define another function that iterates the
previous one along the @${Y} dimension, in order to generate a line of
nodes @${a_{i}}, followed by another line @${b_{i}} displaced to the
centre of the pyramid and to its height, followed by the remaining
lines, until the end, when we have to produce another line @${a_{i}}.
It is still necessary to take into account that the lines @${b_{i}}
have one less node then the lines @${a_{i}}. Based on these
considerations, we can write:
 

@def[
(define (horizontal-pyramid-coordinates p h l m n)
  (if (= m 0)
    (list (line-coordinates p l n))
    (cons
     (line-coordinates p l n)
     (cons
       (line-coordinates 
         (+xyz p (/ l 2) (/ l 2) h) l (- n 1))
       (horizontal-pyramid-coordinates
         (+y p l) h l (- m 1) n)))))
]

We can now combine the list of lists of coordinates created by the previous
function with the one that creates a spatial truss. As an example, the
following expression creates the truss shown in
@figref{fig:trelicaEspacialHorizontal}:

@lispcode[
(spatial-truss
  (horizontal-pyramid-coordinates (xyz 0 0 0) 1 1 8 10))
]

@questions[
@question{ Consider the creation of a @emph{random} spatial
truss. This truss is characterized by its nodes' coordinates being
positioned at a random distance from the correspondent nodes'
coordinates of a horizontal spatial truss, as it is exemplified in the
following figure where, to facilitate the visualization, the struts
that join the @${a_i}, @${b_i} and @${c_i} nodes where highlighted in
the scheme shown in @figref{fig:trelicaLigada}.

 
@fig[@tex{
\begin{tikzpicture}[scale=0.7]
\inputtikz{trelicaAleatoria0}
\end{tikzpicture}
}]

Define the function @fn[random-truss-coordinates] that, in addition
to the @fn[horizontal-pyramid-coordinates] function parameters,
also receives the maximum distance @${r} that each node from the
random truss can be placed relatively to the correspondent node of the
horizontal truss. As an example, consider that the truss presented in
the previous figure was created by the evaluation of the following
expression:

@lispcode[
(spatial-truss
  (random-truss-coordinates (xyz 0 0 0) 1 1 8 10 0.2))
]}


@question{ Consider the creation of arched spatial truss, like the one
presented in the following image (perspective view). Define the
@fn[arched-spatial-truss] function that, besides the
@fn[arc-truss] function parameters, also has an additional
parameter that indicates the number of simple trusses that constitute
the spatial truss. As an example, consider that the truss shown in the
following image was created by the expression:
  
@lispcode[
(arched-spatial-truss (xyz 0 0 0) 10 9 1.0 20 0 -pi/3 pi/3 10)
]

@fig[@tex{
\begin{tikzpicture}[scale=0.6]
\inputtikz{trelicaEspacialArco0}
\end{tikzpicture}
}]
}

@question{ Consider the truss shown in the following image:

@fig[@tex{
\begin{tikzpicture}[scale=0.9]
\inputtikz{trelicaSeno0}
\end{tikzpicture}
}]

This truss is similar to the arched spatial truss but with a
@emph{nuance}: the exterior arc @lisp[rac] and the interior arc
@lisp[rb] vary along the arc axis. This variation corresponds to a
sinusoid of amplitude @${\Delta_r} varying from an initial value
@${\alpha_0} to a final value @${\alpha_1}, with increments of
@${\Delta_\alpha}.

Define the @fn[wave-truss] function that creates these types of
trusses, with the same parameters as the @fn[arched-spatial-truss]
function and also with the parameters @${\alpha_0}, @${\alpha_1},
@${\Delta_\alpha} and @${\Delta_r}. As an example, consider that the
previous figure was created by the following expression:

@lispcode[
(wave-truss (xyz 0 0 0) 10 9 1.0 20 0 -pi/3 pi/3 0 4pi (/ pi 8) 1)
]
}
]