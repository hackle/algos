let greetings = [ "Hello", "Howdy", "Hi", "G'day" ];
let index = 0;

function greet(name: string): string {
    return `${greetings[index++ % greetings.length]} ${name}`;
}

console.log(greet("Hackle"));
console.log(greet("Hackle"));
console.log(greet("Hackle"));
console.log(greet("Hackle"));
console.log(greet("Hackle"));
console.log(greet("Hackle"));