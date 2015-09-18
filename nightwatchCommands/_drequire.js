module.exports = function(filename, throwOnFail) {
  throwOnFail = throwOnFail || true;

  // Search the require cache for the filename.
  for(var mod in require.cache) {
    if(mod.indexOf(filename) !== -1) {
      // Found it, return the required reference.
      return require(require.cache[mod].filename);
      break;
    }
  }

  if(throwOnFail)
    throw new Error("Unable to load dynamically load: " + filename);
};