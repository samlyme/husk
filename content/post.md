---
title: "Imperative vs Declarative Code"
meta_title: ""
description: "this is meta description"
date: 2023-04-04
image: "https://external-preview.redd.it/vC0o6qZu4OlzXFzwQoTRltJ85JT0of4o2Zq5DUAa8wA.jpg?auto=webp&s=15b395e11d37dc36733eccd8f40a5530b502852b"
categories: ["Software Engineering"]
author: "Sam Ly"
tags: ["Programming Paradigms"]
draft: false
---

# Imperative vs Declarative code

The most fundamental difference between imperative and declarative code is that imperative code tells the computer *how* to achieve a certain goal, while declarative code tells the computer *what* a certain goal is, and the computer will achieve that goal for you. Another way to conceptualize this is that with declarative code, the computer "knows" what the goal is, while with imperative code, the computer doesn't know what the end goal is, and is simply following your instructions.

Imagine you have a robot that can make you sandwiches. You tell the robot "make me a PB&J sandwich." The robot obliges and makes you a PB&J. Here, the command that you gave the robot was **declarative** because it told the robot *what* goal to achieve.

If instead, you told the robot "Put a piece of bread on a plate. Spread peanut butter on that piece of bread. Get another piece of bread and put it on the plate. Spread jelly on that piece of bread. Put the pieces of bread together." the command that you gave the robot was **imperative** because the robot does not know what the end goal is, and is just following your instructions.

When writing imperative code, you are **instructing** the computer to get it to perform a task, while with declarative code, you are **describing** the task, or in other words, *declaring* the your intent. 

## Example

Let's say you have an array of strings in JS, and your goal is to print out all strings to the console.

```js
const arr = ["Apple", "Banana", "Pear"];

// Imperative for-loop
for (let i = 0; i < arr.length; i++) {
 console.log(arr[i]);
}

// Declarative for-loop
arr.forEach(
 (s) => console.log(s)
);
```

The first example is imperative because you are instructing the computer to iterate through the array and print out the item at each index, however that program segment doesn't "know" the end goal, which is to print out every string in array `arr`.  The program only knows to go to each index in the array and print out the value. Only you know the end goal, and the code "trusts" you to correctly implement that goal.

The second example is declarative because you tell the computer to print out each item in the array `arr`. The code "knows" the end goal, and the you place trust in the computer that it knows how to handle that goal.

## Pros and cons of each

Because imperative code places trust in the developer, it is more prone to human error.

```js
// The end condition will cause an array out of bounds error
for (let i = 0; i <= arr.length; i++) {
 console.log(arr[i]);
}
```

Using `i <= arr.length` is an extremely common "off by one" error. This only happens because the computer trusts that the developer can implement a specific goal correctly.

However, the greater trust placed in the developer also allows for greater power for the developer. For example, if developer needs to access multiple items in an array in one iteration, imperative code will almost always be written.

Imagine you need to print out the sum of consecutive integers in an array. The imperative implementation is fairly simple.

```js
const arr = [43, 57, 87, 94, 23, 87];

// Imperative
for (let i = 0; i < arr.length - 1; i++) {
 console.log(arr[i] + arr[i+1]);
}
```

Because imperative code places more trust on the developer, it is often more versatile, but can also be more error-prone. On the other hand, because declarative code places less trust in the developer, it is less error prone, but can be limited by the features of the programming language.

## Stateful data

Because imperative code deals with *how* a goal is being accomplished, the management of the data's state is left to the developer. The developer is trusted with ensuring that the state of the data is valid.

For example, in an imperative for-loop iterating over an array, the state of the loop variable determines where in the array a particular iteration of the loop will access. The developer is trusted with properly managing the state of that loop variable to achieve the end goal.

```js
const arr = [43, 57, 87, 94, 23, 87];

// The bounds and management of the state of i is up to you!
for (let i = 0; i < arr.length; i++) {
 // Some code...
}

// You can even set i to an invalid value and break the program!
for (let i = 0; i < arr.length; i++) {
 i = -1;
 console.log(arr[i]);
}
```

Because the code doesn't know what the end goal of the code is, it can't assume anything that you are doing is wrong.

In comparison, iterating over an array using a declarative for-loop places zero trust in the developer to manage the loop variable. The declarative for-loop has a goal, and it will achieve that goal, and the developer is not allowed to interfere.

```js
// This gives the developer less trust so now they can't break it if they tried.
arr.forEach(
 (s) => console.log(s)
);
```

The `.forEach` loop has a clear goal. Its goal is to iterate over each element, so the computer knows what to do. Because the computer knows what to do, the developer is not allowed to interfere.

## Abstraction

Abstraction is when a complex set of operations are bundled together so that they can be used with less effort.

This is code to find the GCD between two numbers `num1` and `num2`.

```js
const num1 = 252;
const num2 = 105;

let a = Math.abs(num1);
let b = Math.abs(num2);

// Calculate GCD
while (a > 0 && b > 0 && a != b) {
 if (a > b) {
  a-=b;
 }
 else {
  b-=a;
 }
}

const GCD = a || b;
```

In this (contrived) example, the developer that needs `num1` and `num2` needs to understand how the GCD calculation works.

```js
// in index.js
import findGCD from "./utils.js";

const num1 = 252;
const num2 = 105;
const GCD = findGCD(num1, num2);
```

```js
// in utils.js file
export default function findGCD(num1, num2) {
   let a = Math.abs(num1);
   let b = Math.abs(num2);
   while (a > 0 && b > 0 && a !== b) {
      if(a > b){
         a-=b;
      }else{
         b-=a;
      };
   };
   return a || b;
}
```

Here, the functionality for finding GCD is abstracted into a function. The name of the function gives other devs a clear indication to the "end goal" of the function. Other devs can now declaratively state that they would like the GCD between two numbers, without having to understand the implementation themselves.

**In a sense, by abstracting away code complexity, we make our code "more declarative". However, this code can't be considered truly declarative.**

### Abstraction Cont

This idea of abstraction is applied everywhere in programming. When you declare a variable, the computer will actually find space in memory, allocate that space, then give access to that memory to the current program thread.

This process is *abstracted* away from the developer. The developer places trust in the computer that it will accomplish the goal of creating a variable, instead of the computer placing trust in the developer to properly manage memory.

The more abstractions, the "higher level" the language. A "high level" language is filled with these abstractions, while a "low level" has less of these abstractions.

Low level languages place more trust in the developer, similar to how imperative code places more trust in the developer, while high level languages place less trust in the developer, similar to how declarative code places less trust in the developer.