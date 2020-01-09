#!/usr/bin/env python
# coding: utf-8

# # Lecture 1: Intro to scientific programming

# ## Basic programming 1:
# 
# - Variables (declaration, assignment)
# - Datatypes (int, float)
# - Operations (add, subtract, multiply, divide)
# - Lists

# ### Variables
# 
# Think of a variable as a name attached to a particular object. In Python, variables need not be declared or defined in advance, as is the case in many other programming languages. To create a variable, you just assign it a value and then start using it. Assignment is done with a single equals sign (=):

# In[37]:


n = 300
print(n)


# Later, if you change the value of n and use it again, the new value will be substituted instead:

# In[38]:


print(n)
n = 1000
print(n)


# ### Data types
# 
# Variables in Python (and other programming languages) have a data 'type'. The 3 most common data types are: integer, float and string. In Python, these types are defined as classes (more on this in the next Lecture). In order to find to which class the variable belongs to you can use type() function.
# 
# Note that floats represent real numbers and are written with a decimal point dividing the integer and the fractional parts. Floats may also be in scientific notation, with E or e indicating the power of 10 (2.5e2 = 2.5 x 102 = 250).

# In[39]:


# This line is commented: anything after the # symbol is not executed 
a = 5 
print(type(a))

b = 5.5
print(type(b))

c = 'Blabla'
print(type(c))


# ### Basic operations
# 
# Python supports all of the math operations that you would expect. The basic ones are addition, subtraction, multiplication, and division. 
# 
# Note that the result of a division is always of type 'float', even if the two numbers divided are integers. Use '//' to keep it an integer.

# In[53]:


# Division
int_a = 6
int_b = 3
print(type(int_a))
print(type(int_b))
print(type(int_a/int_b))
print(type(int_a//int_b))

# Exponential
print(2**5)

# Modulo
print(9%2)


# In[41]:


my_string = 'bla'

# String Cconcatenation
print(my_string + 'BLA')

# String repetition
print(my_string*5)


# ### Lists
# 
# In programming, it is often useful to collect certain variables together under the same name. Lists allow us to do that. A list is a collection of variables that is ordered and changeable. It allows duplicate members.
# 
# For example, if we want to store the ages of 5 people, we could define 5 variables each with a single value. But that would be tedious. Instead we can create a list, with all the ages, as follows:

# In[42]:


all_ages = [10,15,8,34,12]

print(all_ages)
print(type(all_ages))


# One can then access members of the list with the syntax below.
# Note that indexing in Python starts at 0 and not at 1.

# In[43]:


print(all_ages[0]) # first element
print(all_ages[2]) # Element 2 (the third in the list)
print(all_ages[-1]) # last element
print(all_ages[1:3]) # Elements 1 to 3 (1 included, 3 excluded)
print(all_ages[:3]) # Elements up to 3 (3 excluded)
print(all_ages[3:]) # Elements from 3 onwards (3 included)


# ## Exercise 1:

# In[44]:


# 1(a) Make a list called my_list, which contains 2 integers, 2 floats and 1 string. 
# Print the list.
# Print the type of the 4th element.


# In[45]:


# 1(b) Make a new list called 'new_list' that contains all the elments of 'my_list' except the first 2 (use slicing).


# In[46]:


# 1(c) Debug the following code:

my_name = 'R2D' + 2
print(my_name)

# Hint: Google the function 'str'


# ## Basic programming 2:
# 
# - For loops 
# - Logic (boolean type)
# - if...else

# ### For loops
# 
# We often need to do the same operation many times. 'For' loops allow us to just that.

# In[ ]:


fruits = ["apple", "banana", "cherry"]
for x in fruits:
  print(x)

# Note that the name of the variable x doesn't matter. We could also write: 
#for fruit in fruits:
#    print(fruit)


# Note that indentation is important in Python! Anything indented after the for statement will be part of the loop

# ### Logic
# 
# In programming it is often useful to test whether statements are true or false. To do this, Python has a number of logical operators. 
# 
# We can for example test whether a particular variable is greater than another one. The result of that operation should be either true (1) or false (0).

# In[ ]:


low_num = 5
high_num = 10

result = high_num>low_num
print(result)

result = high_num<low_num
print(result)


# Note that the type of the variable 'result' in the cell above is called a Boolean. It can take the value 1 or 0, which Python interprets as either true or false.

# We can also test whether a variable is equal to a particular value using the '==' operator. Careful not to confuse this with the assignment operator '='

# In[ ]:


result = low_num == 5
print(result)


# We can also logically combine several comparison statements. For example with the 'and' operator:

# In[ ]:


result = (low_num == 5) and (high_num>low_num)
print(result)

result = (low_num == 5) and (high_num<low_num)
print(result)


# ### if....else
# 
# Earlier we saw logic statements that were either True or False. We now see how to do a certain operation if a condition is met, and another if it isn't.

# In[ ]:


a = 33
b = 33
if b > a:
  print("b is greater than a")
else: # Note the double equal sign!
  print("b is not greater than a")


# ### Exercise 2

# In[ ]:


# 2(a) Use a for loop to sum all the elements in the following list:

all_ages = [10,15,8,34,12]


# In[ ]:


# 2(b) Modify this code such that only the even numbers are summed.


# ## Basic programming 3:
# 
# - Functions

# A function is a block of code which only runs when it is called. You can pass data, known as parameters, into a function. A function can return data as a result.

# In[ ]:


# Function definition
def print_name(first_name):
    name = first_name + " Refsnes"
    return name
  
# Main program
my_name = print_name("Emil") # Function call

print(my_name)


# By default, a function must be called with the correct number of arguments. Meaning that if your function expects 2 arguments, you have to call the function with 2 arguments, not more, and not less.

# ### Exercise 3

# In[ ]:


# 3.(a) Modify the function 'print_name' above such that it takes 2 arguements: the first and last names, 
# and prints them together. 


# In[ ]:


# 3.(b) Create a function check_range(n,st,en) that returns TRUE if ‘n’ is in the range defined by ‘st’ and ‘en’. 
# and FALSE otherwise. 


# In[ ]:


# 3.(c) Optional. Learn about how to define a function when you don't know the exact number of arguments: 
# https://www.w3schools.com/python/python_functions.asp


# ## Basic programming 4:
# 
# - Numpy arrays

# Numpy is a powerful python package that is often used in scientific computing. More information about packages will be provided in Lecture 2.
# 
# The main purpose of the Numpy package is to allow the creation and manipulation of arrays. A numpy array is a table of elements (usually numbers), all of the same type, indexed by integers. In Numpy, dimensions are called axes.
# 

# ### Array creation
# 
# There are several ways to create arrays.
# 
# For example, you can create an array from a regular Python list or tuple using the array function. The type of the resulting array is deduced from the type of the elements in the sequences.

# In[ ]:


import numpy as np # This line imports the numpy package with the name 'np'. This is a standard way of doing it.

a = np.array([2,3,4])
print(a)
print('The type of a is:' + str(type(a))) # 
print('The type of the elements of a is:' + str(type(a[0]))) # 


# Often, the elements of an array are originally unknown, but its size is known. Hence, NumPy offers several functions to create arrays with initial placeholder content. These minimize the necessity of growing arrays, which is an expensive operation.

# In[ ]:


a = np.zeros( (3,4) )
b = np.ones( (2,3,4), dtype=np.int16 ) # the element type can also be specified through (dtype)

print(a)


# ### Element-wise operations
# 
# Unlike in many matrix languages (e.g Matlab), the product operator * operates elementwise in NumPy arrays. The matrix product can be performed using the @ operator (in python >=3.5) or the dot function:

# In[ ]:


A = np.array( [[1,1], [0,1]] )
B = np.array( [[2,0], [3,4]] )

print(B)


# In[ ]:


C_el = A * B                       # elementwise product
print(C_el)


# In[47]:


C_mat = np.dot(A,B)                    # matrix product
print(C_mat)


# ### Built-in functions
# 
# Numpy provides useful functions such as computing the sum of all the elements in the array.

# In[48]:


my_sum = np.sum(A)
print(my_sum)


# By default, these operations apply to the array as though it were a list of numbers, regardless of its shape. However, by specifying the axis parameter you can apply an operation along the specified axis of an array:

# In[49]:


my_sum = np.sum(A,axis=0)
print(my_sum)


# ### Exercise 4

# In[50]:


# 4(a). Generate a 3x2 array of numbers of your choice using np.array . Check that the dimensions are right


# In[51]:


# 4(b). Write a program to get the values and indices 
# of the elements that are bigger than 3 in your array (use a built-in numpy function). Google is your friend :)


# In[52]:


# 4(c). Write a program that creates a 6*1 numpy array of zeros. Then fill that array with the sequence 1,2,3,4,5,6, 
# using a'for' loop. 

