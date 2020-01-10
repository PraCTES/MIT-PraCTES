clear all; clc; 

%% Lecture 1: Intro to scientific programming (Matlab version)

% Meta: As in python, users can choose to interact with matlab in several
% different modes, depending on their needs:
%
% 1. the Command Window allows is the most basic mode, where you can submit
% one or several lines of code at a time
%
% 2. a matlab script (.m file), which can be run either all at once (by
% pressing the "Run" button in the Matlab toolbar or by running the
% scipt from the command line by calling `matlab [script name].m`) or
% can be run section by section via the MATLAB Desktop environment.
%
% 3. a matlab live script (.mlx file) which can be run in the MATLAB
% Live Editor, which allows outputs to be embedded within a script
% and automatically turns matlab comments into nicely-formatted markdown
% text.

%% Basic programming 1: variables, types, operations, and lists
%
% - Variables (declaration, assignment)
% - Datatypes (int64, double, chat)
% - Operations (add, subtract, multiply, divide)
% - Lists

%% Variables
% Think of a variable as a name attached to a particular object. In Matlab,
% variables need not be declared or defined in advance, as is the case in
% many other programming languages. To create a variable, you just assign
% it a value and then start using it. Assignment is done with a single
% equals sign (=):

n = 300 % by default Matlab prints the value of a variable at assignment

disp(n) % we can use the disp() function to display it at any time

% Later, if you change the value of n and use it again, the new value will
% be substituted instead:

n = 1000; % putting a semicolon after an assignment prevents displaying it

%% Data types
%
% Variables in Matlab (and other programming languages) have a data 'type'
% or class.
%
% The 3 most common data types are: int64, double, and char. In Matlab,
% these types are defined as classes (more on this in the next Lecture).
% In order to find to which class the variable belongs to you can use
% class() function.
%
% Note that doubles represent real numbers and are written with a decimal
% point dividing the integer and the fractional parts. 
%
% Floats may also be in scientific notation, with E or e indicating the
% power of 10 (2.5e2 = 2.5 x 102 = 250).
%
% In matlab, in contrast to Python, all numbers default to double. You can
% convert a rounded double to an int64 (integer) with the int64() function.

a = 5
class(a) % calling a variable or function without a semi-colon displays it

int_a = int64(5)
class(int_a)

b = 5.5
class(b)

c = 'Blabla'
class(c)


%% Basic operations
%
% Matlab supports all of the math operations that you would expect.
% The basic ones are addition, subtraction, multiplication, and division.

% In[53]:


% Division
int_a = 6
int_b = 3
class(int_a)
class(int_b)
class(int_a/int_b)

% Exponential
2^5

% Modulo
mod(9,2)

my_string = 'bla'

% String Concatenation
str_cat = [my_string,'BLA']


%% Lists
%
% In programming, it is often useful to collect certain variables together
% under the same name. Lists allow us to do that. 
% A list is a collection of variables that is ordered and changeable.
% It allows duplicate members.
% For example, if we want to store the ages of 5 people, we could define
% 5 variables each with a single value. But that would be tedious. 
% Instead we can create a list, with all the ages, as follows:


all_ages = [10,15,8,34,12]
class(all_ages)


% One can then access members of the list with the syntax below.
% Note that indexing in Matlab starts at 0 and not at 1.

disp(all_ages(1)) % first element
disp(all_ages(3)) % Element 3 (the third in the list)
disp(all_ages(end)) % last element
disp(all_ages(1:3)) % Elements 1 to 3 (1 included, 3 included)
disp(all_ages(1:3)) % Elements up to 3 (3 included)
disp(all_ages(3:end)) % Elements from 3 onwards (3 included)


%% Exercise 1:

% 1(a) Make a list called my_list, which contains 2 int64s, 2 doubles and
% 1 char.
% disp the list.
% disp the type of the 4th element.

% 1(b) Make a new list called 'new_list' that contains all the elments of
% 'my_list' except the first 2 (use slicing).

%% Basic programming 2: loops, Boolean logic, and conditions

% - For loops
% - Logic (boolean type)
% - if...else

% %%% For loops
%
% We often need to do the same operation many times. 'For' loops allow us
% to just that.

fruits = ["apple", "banana", "cherry"];
for i=1:length(fruits)
  fruits(i)
end


%% Logic
%
% In programming it is often useful to test whether statements are true or
% false. To do this, Matlab has a number of logical operators.
%
% We can for example test whether a particular variable is greater than
% another one. The result of that operation should be either true (1) or
% false (0).

low_num = 5
high_num = 10

greater_than_result = high_num>low_num

lesser_than_result = high_num<low_num


% Note that the tyoe of the variable 'result' in the cell above is called
% a `logical`. It can take the value 1 or 0, which Matlab interprets as
% either true or false.

% We can also test whether a variable is equal to a particular value using
% the '==' operator. Careful not to confuse this with the assignment
% operator '='

equal_result = low_num == 5

% We can also logically combine several comparison statements. For example
% with the '&&' (and) operator:


equal_and_greater_result = (low_num == 5) && (high_num>low_num)

equal_and_lesser_result = (low_num == 5) && (high_num<low_num)


%% if....else
%
% Earlier we saw logic statements that were either True or False. We now see how to do a certain operation if a condition is met, and another if it isn't.


a = 33
b = 33
if b > a
  disp("b is greater than a")
else 
  disp("b is not greater than a")
end


%% Exercise 2

% 2(a) Use a `for loop` to sum all the elements in the following list:

all_ages = [10,15,8,34,12];

% 2(b) Modify this code such that only the even numbers are summed.


%% Basic programming 3:
%
% - Functions

% A function is a block of code which only runs when it is called. You can
% pass data, known as parameters, into a function. A function can return
% data as a result.
% 
% In Matlab the functions must be at the end of the file. See the end of
% this file for the function definition of 'disp_name'.
% 
% Alternatively, functions can be contained in another script, where the
% convention is that the the function [function_name]() must be contained
% in a script named `[function_name].m`

% Main program
my_name = disp_name("Emil"); % Function call
my_name

% By default, a function must be called with the correct number of
% arguments. Meaning that if your function expects 2 arguments, you have
% to call the function with 2 arguments, not more, and not less.

%% Exercise 3

% 3.(a) Modify the function 'disp_name' such that it takes 2
% arguements: the first and last names, and disps them together.

% 3.(b) Create a function check_range(n,st,en) that returns TRUE if `n` is
% in the range defined by `st` and `en` and FALSE otherwise.

% 3.(c) Optional. Learn about how to define a function when you don't know
% the exact number of arguments:
% https://www.w3schools.com/Matlab/Matlab_functions.asp


%% Basic programming 4:
%
% Arrays 

% Much as lists of doubles (e.g. [1.2, 3.5, 8.5] are 1-dimensional
% generalizations of 0-dimensional matlab doubles, matlab double arrays
% are N-dimensional (finite) generalizations of matlab doubles.
% Note: 2-dimensional arrays are often refered to as matrices.

% Example 2-dimensional array / matrix

a = [2,3,4]
class(a)
size(a)

%%% Array creation

% Often, the elements of an array are originally unknown, but its size is
% known. Hence, NumPy offers several functions to create arrays with
% initial placeholder content. These minimize the necessity of growing
% arrays, which is an expensive operation.

a = zeros(3,4)
b = ones(2,3,4) % the element type can also be specified through (dtype)

% %%% Element-wise operations
%
% Unlike in many matrix languages (e.g Matlab), the product operator * operates elementwise in NumPy arrays. The matrix product can be performed using the @ operator (in Matlab >=3.5) or the dot function:


A = [1,1;0,1]
B = [2,0;3,4]

C_el = A .* B % elementwise product


C_mat = A*B % matrix product


%% Built-in functions

my_sum = sum(A(:))

my_sum = sum(A,1)

%% Exercise 4

% 4(a). Generate a 3x2 array of numbers of your choice. Check that the
% dimensions are correct.

% 4(b). Write a program to get the values and indices of the elements that
% are greater than 3 in your array (use a built-in function).
% Google is your friend :)

% 4(c). Write a program that creates a 6x1 dimension array of zeros.
% Then fill that array with the sequence 1,2,3,4,5,6,
% using a `for loop`.


%% Function definition
function name = disp_name(first_name)
    name = first_name + " Refsnes";
end
