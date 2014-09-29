package org.openmole.misc.tools.io;

import org.openmole.misc.tools.io.visitor.CopyDirVisitor;
import org.openmole.misc.tools.io.visitor.DeleteDirVisitor;

import java.io.IOException;
import java.nio.file.*;
import java.util.EnumSet;
import java.util.Set;
import java.util.Objects;


public class DirUtils {
  /**
   * Copies a directory tree
   * @param from
   * @param to
   * @throws IOException
   *    
   */
  public static void copy(Path from, Path to, Set<FileVisitOption> visitOption, CopyOption... copyOptions) throws IOException {
    validate(from);
    Files.walkFileTree(from, visitOption, Integer.MAX_VALUE, new CopyDirVisitor(from, to, copyOptions));
  }
  
  /**
   * Copies a directory tree with default options
   * @param from
   * @param to
   * @throws IOException
   *    
   */
  public static void copy(Path from, Path to) throws IOException {
    copy(from, to, EnumSet.noneOf(FileVisitOption.class), LinkOption.NOFOLLOW_LINKS, StandardCopyOption.COPY_ATTRIBUTES);
  }

  /**
   * Moves one directory tree to another.  Not a true move operation in that the
   * directory tree is copied, then the original directory tree is deleted.
   *
   * @param from
   * @param to
   * @throws IOException
   */
  public static void move(Path from, Path to) throws IOException {
    validate(from);
    Files.walkFileTree(from, new CopyDirVisitor(from, to));
    Files.walkFileTree(from, new DeleteDirVisitor());
  }

  /**
   * Completely removes given file tree starting at and including the given path.
   *
   * @param path
   * @throws IOException
   */
  public static void delete(Path path) throws IOException {
    validate(path);
    Files.walkFileTree(path, new DeleteDirVisitor());
  }


  /**
   * If the path exists, completely removes given file tree starting at and including the given path.
   *
   * @param path
   * @throws IOException
   */
  public static void deleteIfExists(Path path) throws IOException {
    if (Files.exists(path))   delete(path);
  }

  private static void validate(Path... paths) {
    for (Path path : paths) {
      Objects.requireNonNull(path);
      if (!Files.isDirectory(path)) {
        throw new IllegalArgumentException(String.format("%s is not a directory", path.toString()));
      }
    }
  }
}

