import { Transpiler } from './src/transpiler.js';
import * as fs from 'fs';
import { IInput } from './src/types.js';
const { readFileSync, writeFileSync } = fs;

const transpiler = new Transpiler({
    julia: {
        uncamelcaseIdentifiers: true,
    },
});

// console.log(transpiler.juliaTranspiler)
// const tsinput = "// This is a comment\n/* Multi-line\ncomment */";
const tsinput = `function fetchStatus(params) {
    /**
        * @method
        * @name aax#fetchStatus
        * @description the latest known information on the availability of the exchange API
        * @param {object} params extra parameters specific to the aax api endpoint
        * @returns {object} a [status structure]{@link https://docs.ccxt.com/en/latest/manual.html#exchange-status-structure}
        */
    return 1;
}`;

// transpiler.setJuliaUncamelCaseIdentifiers(true);
const transpiled = transpiler.transpileJulia(tsinput);

console.log(transpiled.content); // prints my_var = 1class
